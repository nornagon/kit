package kit

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

// Adapted from https://github.com/d3/d3-delaunay/blob/0b51543/src/voronoi.js
// Copyright 2018-2021 Observable, Inc. and Jeremy Rose
//
// Permission to use, copy, modify, and/or distribute this software for any purpose
// with or without fee is hereby granted, provided that the above copyright notice
// and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
// OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
// TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
// THIS SOFTWARE.
object VoronoiFromDelaunay {
  def fromPoints(points: Seq[Vec2], aabb: AABB): VoronoiFromDelaunay =
    new VoronoiFromDelaunay(Delaunator.fromPoints(points), aabb)
}
class VoronoiFromDelaunay(delaunay: Delaunator, aabb: AABB) {
  private val triangles = delaunay.triangles
  private val points = delaunay.coords
  private val circumcenters: Seq[Vec2] = (triangles.indices by 3).map { i =>
    val t1 = triangles(i) * 2
    val t2 = triangles(i + 1) * 2
    val t3 = triangles(i + 2) * 2
    val x1 = points(t1)
    val y1 = points(t1 + 1)
    val x2 = points(t2)
    val y2 = points(t2 + 1)
    val x3 = points(t3)
    val y3 = points(t3 + 1)

    val dx = x2 - x1
    val dy = y2 - y1
    val ex = x3 - x1
    val ey = y3 - y1
    val ab = (dx * ey - dy * ex) * 2

    if (math.abs(ab) < 1e-9) {
      // degenerate case (collinear diagram)
      // almost equal points (degenerate triangle)
      // the circumcenter is at the infinity, in a
      // direction that is:
      // 1. orthogonal to the halfedge.
      var a = 1e9
      // 2. points away from the center; since the list of triangles starts
      // in the center, the first point of the first triangle
      // will be our reference
      val r = triangles(0) * 2
      a *= math.signum((points(r) - x1) * ey - (points(r + 1) - y1) * ex)
      Vec2(
        (x1 + x3) / 2 - a * ey,
        (y1 + y3) / 2 + a * ex
      )
    } else {
      val d = 1 / ab
      val bl = dx * dx + dy * dy
      val cl = ex * ex + ey * ey
      Vec2(
        x1 + (ey * bl - dy * cl) * d,
        y1 + (dx * cl - ex * bl) * d
      )
    }
  }

  private val inedges = Array.fill[Int](points.length / 2)(-1)
  for (e <- delaunay.halfedges.indices) {
    val p = delaunay.triangles(if (e % 3 == 2) e - 2 else e + 1)
    if (delaunay.halfedges(e) == -1 || inedges(p) == -1) inedges(p) = e
  }

  private val hullIndex = Array.fill[Int](points.length / 2)(-1)
  for ((h, i) <- delaunay.hull.zipWithIndex)
    hullIndex(h) = i

  private val vectors = new Array[Double](points.length * 2)

  {
    var h = delaunay.hull(delaunay.hull.length - 1)
    var p0 = 0
    var p1 = h * 4
    var x0 = 0d
    var x1 = points(2 * h)
    var y0 = 0d
    var y1 = points(2 * h + 1)

    for (i <- delaunay.hull.indices) {
      h = delaunay.hull(i)
      p0 = p1
      x0 = x1
      y0 = y1
      p1 = h * 4
      x1 = points(2 * h)
      y1 = points(2 * h + 1)
      vectors(p1) = y0 - y1
      vectors(p0 + 2) = y0 - y1
      vectors(p1 + 1) = x1 - x0
      vectors(p0 + 3) = x1 - x0
    }
  }

  def cellPolygon(i: Int): Option[Polygon] = {
    val polygon = _clip(i)
    if (polygon == null || polygon.points.isEmpty) return None
    var prev = polygon.points.last
    val seq = mutable.Buffer.empty[Vec2]
    for (p <- polygon.points) {
      if (p != prev)
        seq += p
      prev = p
    }
    Some(Polygon(seq))
  }

  def cellPolygons(): Iterable[Polygon] =
    (0 until (points.length / 2)) flatMap cellPolygon

  private def _cell(i: Int): Seq[Vec2] = {
    val e0 = inedges(i)
    if (e0 == -1) return null // coincident point
    var points = Seq.empty[Vec2]
    var e = e0
    do {
      val t = math.floor(e / 3).toInt
      points :+= circumcenters(t)
      e = if (e % 3 == 2) e - 2 else e + 1
      if (triangles(e) != i) return points // bad triangulation
      e = delaunay.halfedges(e)
    } while (e != e0 && e != -1)
    points
  }

  private def _clip(i: Int): Polygon = {
    // degenerate case (1 valid point: return the box)
    if (i == 0 && delaunay.hull.length == 1) {
      return aabb.toPolygon
    }
    val points = _cell(i)
    if (points == null) return null
    val v = i * 4
    if (vectors(v) != 0 || vectors(v + 1) != 0)
      _clipInfinite(i, points, vectors(v), vectors(v + 1), vectors(v + 2), vectors(v + 3))
    else
      _clipFinite(i, points)
  }

  private def _clipFinite(i: Int, points: Seq[Vec2]): Polygon = {
    val n = points.size
    var p: mutable.Buffer[Vec2] = null
    var v0: Vec2 = null
    var v1 = points.last
    var c0 = 0
    var c1 = _regioncode(v1)
    var e0 = 0
    var e1 = 0
    for (j <- 0 until n) {
      breakable {
        v0 = v1
        v1 = points(j)
        c0 = c1
        c1 = _regioncode(v1)
        if (c0 == 0 && c1 == 0) {
          e0 = e1
          e1 = 0
          if (p != null) p += v1
          else p = mutable.Buffer(v1)
        } else {
          val (s0, s1) = if (c0 == 0) {
            val s = _clipSegment(v0, v1, c0, c1)
            if (s == null) break
            s
          } else {
            val s = _clipSegment(v1, v0, c1, c0)
            if (s == null) break
            val (s1, s0) = s
            e0 = e1
            e1 = _edgecode(s0)
            if (e0 != 0 && e1 != 0) _edge(i, e0, e1, p, p.size)
            if (p != null) p += s0
            else p = mutable.Buffer(s0)
            (s0, s1)
          }
          e0 = e1
          e1 = _edgecode(s1)
          if (e0 != 0 && e1 != 0) _edge(i, e0, e1, p, p.size)
          if (p != null) p += s1
          else p = mutable.Buffer(s1)
        }
      }
    }
    if (p != null) {
      e0 = e1
      e1 = _edgecode(p.head)
      if (e0 != 0 && e1 != 0) _edge(i, e0, e1, p, p.size)
    } else if (contains(i, aabb.center)) {
      return aabb.toPolygon
    }
    Polygon(p)
  }

  private def _clipSegment(_v0: Vec2, _v1: Vec2, _c0: Int, _c1: Int): (Vec2, Vec2) = {
    var v0 = _v0
    var v1 = _v1
    var c0 = _c0
    var c1 = _c1
    while (c0 != 0 || c1 != 0) {
      if ((c0 & c1) != 0) return null
      val c = if (c0 != 0) c0 else c1
      val v =
        if ((c & 0x8) != 0)
          Vec2(v0.x + (v1.x - v0.x) * (aabb.upper.y - v0.y) / (v1.y - v0.y), aabb.upper.y)
        else if ((c & 0x4) != 0)
          Vec2(v0.x + (v1.x - v0.x) * (aabb.lower.y - v0.y) / (v1.y - v0.y), aabb.lower.y)
        else if ((c & 0x2) != 0)
          Vec2(aabb.upper.x, v0.y + (v1.y - v0.y) * (aabb.upper.x - v0.x) / (v1.x - v0.x))
        else
          Vec2(aabb.lower.x, v0.y + (v1.y - v0.y) * (aabb.lower.x - v0.x) / (v1.x - v0.x))
      if (c0 != 0) {
        v0 = v
        c0 = _regioncode(v0)
      } else {
        v1 = v
        c1 = _regioncode(v1)
      }
    }
    (v0, v1)
  }

  def contains(i: Int, v: Vec2): Boolean = {
    def _step(i: Int, v: Vec2): Int = {
      if (inedges(i) == -1 || points.length == 0) return (i + 1) % (points.length >> 1)
      var c = i
      var dc = math.pow(v.x - points(i * 2), 2) + math.pow(v.y - points(i * 2 + 1), 2)
      val e0 = inedges(i)
      var e = e0
      do {
        val t = delaunay.triangles(e)
        val dt = math.pow(v.x - points(t * 2), 2) + math.pow(v.y - points(t * 2 + 1), 2)
        if (dt < dc) {
          dc = dt
          c = t
        }
        e = if (e % 3 == 2) e - 2 else e + 1
        if (triangles(e) != i) return c // bad triangulation
        e = delaunay.halfedges(e)
        if (e == -1) {
          e = delaunay.hull((hullIndex(i) + 1) % delaunay.hull.length);
          if (e != t) {
            if (math.pow(v.x - points(e * 2), 2) + math.pow(v.y - points(e * 2 + 1), 2) < dc) return e
          }
          return c
        }
      } while (e != e0)
      c
    }

    _step(i, v) != 0
  }

  private def _clipInfinite(i: Int, points: Seq[Vec2], vx0: Double, vy0: Double, vxn: Double, vyn: Double): Polygon = {
    var p: Vec2 = null
    var P = points.to[mutable.Buffer]
    p = _project(P(0), vx0, vy0)
    if (p != null) P.insert(0, p)
    p = _project(P.last, vxn, vyn)
    if (p != null) P += p
    P = _clipFinite(i, P).points.to[mutable.Buffer]
    if (P != null) {
      var c0: Int = 0
      var c1 = _edgecode(P.last)
      var j = 0
      var n = P.size
      while (j < n) {
        c0 = c1
        c1 = _edgecode(P(j))
        if ((c0 != 0) && (c1 != 0)) {
          j = _edge(i, c0, c1, P, j)
          n = P.size
        }
        j += 1
      }
    } else if (contains(i, aabb.center)) {
      return aabb.toPolygon
    }
    Polygon(P)
  }

  private def _edge(i: Int, _e0: Int, e1: Int, p: mutable.Buffer[Vec2], _j: Int): Int = {
    var j = _j
    var e0 = _e0
    while (e0 != e1) {
      var v: Vec2 = null
      e0 match {
        case 0x5 => e0 = 0x4
        case 0x4 => e0 = 0x6; v = Vec2(aabb.upper.x, aabb.lower.y)
        case 0x6 => e0 = 0x2
        case 0x2 => e0 = 0xa; v = Vec2(aabb.upper.x, aabb.upper.y)
        case 0xa => e0 = 0x8
        case 0x8 => e0 = 0x9; v = Vec2(aabb.lower.x, aabb.upper.y)
        case 0x9 => e0 = 0x1
        case 0x1 => e0 = 0x5; v = Vec2(aabb.lower.x, aabb.lower.y)
      }
      if (v != null) {
        if (j >= p.size || (p(j) != v && contains(i, v))) {
          p.insert(j, v)
          j += 1
        }
      }
    }
    if (p.size > 4) {
      var i = 0
      while (i < p.size) {
        val j = (i + 1) % p.size
        val k = (i + 2) % p.size
        if ((p(i).x == p(j).x && p(j).x == p(k).x) || (p(i).y == p(j).y && p(j).y == p(k).y)) {
          p.remove(j, 1)
        } else {
          i += 1
        }
      }
    }
    j
  }

  private def _project(v0: Vec2, vx: Double, vy: Double): Vec2 = {
    var t = Double.PositiveInfinity
    var v: Vec2 = null
    if (vy < 0) { // top
      if (v0.y <= aabb.lower.y) return null
      val c = (aabb.lower.y - v0.y) / vy
      if (c < t) {
        t = c
        v = Vec2(v0.x + t * vx, aabb.lower.y)
      }
    } else if (vy > 0) { // bottom
      if (v0.y >= aabb.upper.y) return null
      val c = (aabb.upper.y - v0.y) / vy
      if (c < t) {
        t = c
        v = Vec2(v0.x + t * vx, aabb.upper.y)
      }
    }
    if (vx > 0) { // right
      if (v0.x >= aabb.upper.x) return null
      val c = (aabb.upper.x - v0.x) / vx
      if (c < t) {
        t = c
        v = Vec2(aabb.upper.x, v0.y + t * vy)
      }
    } else if (vx < 0) { // left
      if (v0.x <= aabb.lower.x) return null
      val c = (aabb.lower.x - v0.x) / vx
      if (c < t) {
        t = c
        v = Vec2(aabb.lower.x, v0.y + t * vy)
      }
    }
    v
  }

  private def _edgecode(v: Vec2): Int = {
    (
      if (v.x == aabb.lower.x) 0x1
      else if (v.x == aabb.upper.x) 0x2
      else 0
    ) | (
      if (v.y == aabb.lower.y) 0x4
      else if (v.y == aabb.upper.y) 0x8
      else 0
    )
  }
  private def _regioncode(v: Vec2): Int = {
    (
      if (v.x < aabb.lower.x) 0x1
      else if (v.x > aabb.upper.x) 0x2
      else 0
    ) | (
      if (v.y < aabb.lower.y) 0x4
      else if (v.y > aabb.upper.y) 0x8
      else 0
    )
  }
}

// TODO: this is slow and sometimes wrong
class Voronoi(sites: Seq[Vec2]) {
  private val lexicographicYX = new Ordering[Vec2] {
    override def compare(a: Vec2, b: Vec2): Int = {
      val dy = math.signum(b.y - a.y).toInt
      if (dy != 0) dy else math.signum(b.x - a.x).toInt
    }
  }

  val sortedSites = sites.sorted(lexicographicYX.reverse)

  var nextBeachId = 0
  case class Beach(
    site: Vec2,
    id: Int = { nextBeachId += 1; nextBeachId }
  )

  val beaches = mutable.Buffer[Beach]()
  val vertices = mutable.Buffer[Vec2]()

  case class Edge(var start: Vec2, var end: Vec2)
  val edges: mutable.Map[(Vec2, Vec2), Edge] = {
    val underlying = mutable.Map[(Vec2, Vec2), Edge]()
    underlying.withDefault { k =>
      val e = Edge(null, null)
      underlying(k) = e
      e
    }
  }

  val siteEvents = mutable.Buffer[Vec2]() ++ sortedSites
  def nextSite: Option[Vec2] = siteEvents.headOption
  def nextEventY: Option[Double] = {
    val nc = nextCircle()
    val ns = nextSite
    if (nc.isEmpty && ns.isEmpty) return None
    if (ns.isDefined && (nc.isEmpty || nc.get._2.c.y + nc.get._2.r > ns.get.y)) {
      Some(ns.get.y)
    } else Some(nc.get._2.c.y + nc.get._2.r)
  }
  def step(): Boolean = {
    val nc = nextCircle()
    val ns = nextSite
    if (nc.isEmpty && ns.isEmpty) return true
    if (ns.isDefined && (nc.isEmpty || nc.get._2.c.y + nc.get._2.r > ns.get.y)) {
      val site = siteEvents.remove(0)
      addBeach(site)
    } else {
      val Some((beach, circle)) = nc
      val idx = beaches.indexOf(beach)
      val leftBeach = beaches(idx-1)
      val rightBeach = beaches(idx+1)
      val disappearingBeach = beaches.remove(idx)
      val vertex = circle.c
      vertices.append(vertex)
      edges((disappearingBeach.site, leftBeach.site)).start = vertex
      edges((leftBeach.site, disappearingBeach.site)).end = vertex
      edges((rightBeach.site, disappearingBeach.site)).start = vertex
      edges((disappearingBeach.site, rightBeach.site)).end = vertex
      edges((leftBeach.site, rightBeach.site)).start = vertex
      edges((rightBeach.site, leftBeach.site)).end = vertex
    }
    false
  }

  def nextCircle(): Option[(Beach, Circle2)] = {
    val cs = circles()
    if (cs.isEmpty) None
    else Some(cs.minBy { case (_, c) => c.c.y + c.r })
  }

  def circles(): Iterator[(Beach, Circle2)] = {
    // TODO: rather than recomputing all the circles each time, add/remove them during sweep
    for {
      Seq(l, m, r) <- beaches.sliding(3).filter(_.size == 3)
        if l.site != r.site
        if l.site.y > m.site.y || r.site.y > m.site.y
        b = m.site
        a = m.site -> l.site
        c = m.site -> r.site
        d = 2 * (a cross c)
        if d < 0
    } yield {
      val ha = a.lengthSquared
      val hc = c.lengthSquared
      val vec = Vec2(c.y * ha - a.y * hc, a.x * hc - c.x * ha) / d
      (m, Circle2(vec + b, vec.length))
    }
  }

  def addBeach(site: Vec2): Unit = {
    val Vec2(x, directrix) = site
    val beachIdx = findBeachIdxAtX(x, directrix)
    beachIdx match {
      case None =>
        beaches.insert(0, Beach(site))
      case Some(idx) =>
        beaches.insert(idx, Beach(beaches(idx).site), Beach(site))
    }
  }

  /**
    * Returns the list of break points between beaches, starting at NegativeInfinity and ending at PositiveInfinity.
    * @param directrix common directrix
    * @return sequence of break points
    */
  def beachProjections(directrix: Double): Seq[Double] = {
    if (beaches.isEmpty) return Seq.empty
    val breakPoints = beaches.zip(beaches.tail).map { case (a, b) => leftBreakPoint(a.site, b.site, directrix) }
    (Double.NegativeInfinity +: breakPoints :+ Double.PositiveInfinity)(collection.breakOut)
  }

  def completeEdges = for ((k, e) <- edges; if e.start != null && e.end != null) yield k -> e

  def cells(): Map[Vec2, Polygon] = {
    val edgesBySite = mutable.Map[Vec2, Map[Vec2, Vec2]]().withDefaultValue(Map())
    for (((a, b), Edge(start, end)) <- completeEdges) {
      edgesBySite(a) += ((start, end))
      edgesBySite(b) += ((end, start))
    }
    (for ((site, edges) <- edgesBySite; if edges.values.forall(end => edges.contains(end))) yield {
      val first = edges.keys.head
      var p = first
      val points = for (_ <- 1 to edges.size) yield {
        val x = p
        p = edges(p)
        x
      }
      site -> Polygon(points)
    })(collection.breakOut)
  }

  def connectedEdges(bb: AABB): Seq[Edge] = edges.flatMap {
    case ((l, r), e) => connectEdge(l, r, e, bb)
  }.toSeq

  def connectEdge(left: Vec2, right: Vec2, edge: Edge, bb: AABB): Option[Edge] = {
    if (edge.end != null) return Some(edge)
    val Vec2(rx, ry) = left
    val Vec2(lx, ly) = right
    val Vec2(fx, fy) = (left + right) / 2
    var v0 = edge.start
    var v1 = edge.end
    if (ry == ly) {
      if (fx < bb.lower.x || fx >= bb.upper.x) return None
      if (lx > rx) {
        if (v0 == null) v0 = Vec2(fx, bb.lower.y)
        else if (v0.y >= bb.upper.y) return None
        v1 = Vec2(fx, bb.upper.y)
      } else {
        if (v0 == null) v0 = Vec2(fx, bb.upper.y)
        else if (v0.y < bb.lower.y) return None
        v1 = Vec2(fx, bb.lower.y)
      }
    } else {
      val fm = (lx - rx) / (ry - ly)
      val fb = fy - fm * fx
      if (fm < -1 || fm > 1) {
        if (lx > rx) {
          if (v0 == null) v0 = Vec2((bb.lower.y - fb) / fm, bb.lower.y)
          else if (v0.y >= bb.upper.y) return None
          v1 = Vec2((bb.upper.y - fb) / fm, bb.upper.y)
        } else {
          if (v0 == null) v0 = Vec2((bb.upper.y - fb) / fm, bb.upper.y)
          else if (v0.y < bb.lower.y) return None
          v1 = Vec2((bb.lower.y - fb) / fm, bb.lower.y)
        }
      } else {
        if (ly < ry) {
          if (v0 == null) v0 = Vec2(bb.lower.x, fm * bb.lower.x + fb)
          else if (v0.x >= bb.upper.x) return None
          v1 = Vec2(bb.upper.x, fm * bb.upper.x + fb)
        } else {
          if (v0 == null) v0 = Vec2(bb.upper.x, fm * bb.upper.x + fb)
          else if (v0.x < bb.lower.x) return None
          v1 = Vec2(bb.lower.x, fm * bb.lower.x + fb)
        }
      }
    }
    Some(Edge(v0, v1))
  }

  /**
    * Finds the beach whose parabola has the highest y value at the given x.
    * i.e. find the beach 'under' the point given by (x=x, y=directrix).
    */
  def findBeachIdxAtX(x: Double, directrix: Double): Option[Int] = {
    // TODO: this is slow. use a balanced binary search tree
    def leftEdgeOf(i: Int): Double = {
      if (i == 0) Double.NegativeInfinity
      else leftBreakPoint(beaches(i-1).site, beaches(i).site, directrix)
    }
    def rightEdgeOf(i: Int): Double = {
      if (i == beaches.size - 1) Double.PositiveInfinity
      else leftBreakPoint(beaches(i).site, beaches(i+1).site, directrix)
    }
    beaches.indices.find(i => leftEdgeOf(i) < x && x <= rightEdgeOf(i) )
  }

  /**
    * Finds the intersection of two parabolae with foci at `leftFocus` and `rightFocus` and a common directrix.
    *
    * The two foci are presumed to have y <= directrix. If there are two intersection points (the usual case), the one
    * returned will be the intersection which has the parabola focused at `leftFocus` having greater Y value to the left
    * of the intersection point. That is, given the following two parabolae A and B:
    *
    *    B|    /
    *     |   /|Q
    * A\  |  / |
    *   \  \/_/
    *    \_/P
    *
    * The point P would be returned for `leftBreakPoint(A, B)`, while the point Q would be returned for
    * `leftBreakPoint(B, A)`.
    *
    * Cribbed from d3.js: https://github.com/d3/d3-voronoi/blob/a08f96556af848ffe5fcbbc89d1b296abef34986/src/Beach.js#L161
    *
    * @param leftFocus the focus of the parabola on the left of the desired intersection point
    * @param rightFocus the focus of the parabola on the right of the desired intersection point
    * @param directrix the common directrix
    * @return the 'break point' between `leftFocus` on the left and `rightFocus` on the right
    */
  def leftBreakPoint(leftFocus: Vec2, rightFocus: Vec2, directrix: Double): Double = {
    val Vec2(rfocx, rfocy) = rightFocus
    val pby2 = rfocy - directrix
    if (pby2 == 0) return rfocx

    val Vec2(lfocx, lfocy) = leftFocus
    val plby2 = lfocy - directrix
    if (plby2 == 0) return lfocx

    val hl = lfocx - rfocx
    val aby2 = 1 / pby2 - 1 / plby2
    val b = hl / plby2
    if (aby2 != 0)
      (-b + math.sqrt(b * b - 2 * aby2 * (hl * hl / (-2 * plby2) - lfocy + plby2 / 2 + rfocy - pby2 / 2))) / aby2 + rfocx
    else
      (rfocx + lfocx) / 2
  }
}



