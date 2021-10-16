package kit

import java.util
import scala.util.control.Breaks.{break, breakable}

// Adapted from https://github.com/ricardomatias/delaunator/blob/main/src/main/kotlin/com/github/ricardomatias/Delaunator.kt
/*
ISC License

Copyright (c) 2021, Ricardo Matias and Jeremy Rose

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
/**
   * A Kotlin port of Mapbox's Delaunator incredibly fast JavaScript library for Delaunay triangulation of 2D points.
   *
   * @description Port of Mapbox's Delaunator (JavaScript) library - https://github.com/mapbox/delaunator
   * @property coords flat positions' array - [x0, y0, x1, y1..]
 * @since f0ed80d - commit
   * @author Ricardo Matias
   */
class Delaunator(val coords: Array[Double]) {
  import Delaunator._

  private val count = coords.length / 2

  // arrays that will store the triangulation graph
  val maxTriangles: Int = (2 * count - 5).max(0)
  private val _triangles = new Array[Int](maxTriangles * 3)
  private val _halfedges = new Array[Int](maxTriangles * 3)

  var triangles: Array[Int] = _
  var halfedges: Array[Int] = _

  // temporary arrays for tracking the edges of the advancing convex hull
  private val hashSize = math.ceil(math.sqrt(count * 1.0)).floor.toInt
  private val hullPrev = new Array[Int](count) // edge to prev edge
  private val hullNext = new Array[Int](count) // edge to next edge
  private val hullTri = new Array[Int](count) // edge to adjacent triangle
  private val hullHash = new Array[Int](hashSize) // angular edge hash
  private var hullStart: Int = -1

  // temporary arrays for sorting points
  private val ids = new Array[Int](count)
  private val dists = new Array[Double](count)

  private var cx: Double = Double.NaN
  private var cy: Double = Double.NaN

  private var trianglesLen: Int = -1

  var hull: Array[Int] = _

  update()

  def update(): Unit = {
    // populate an array of point indices calculate input data bbox
    var minX = Double.PositiveInfinity
    var minY = Double.PositiveInfinity
    var maxX = Double.NegativeInfinity
    var maxY = Double.NegativeInfinity

    // points -> points
    // minX, minY, maxX, maxY
    for (i <- 0 until count) {
      val x = coords(2 * i)
      val y = coords(2 * i + 1)
      if (x < minX) minX = x
      if (y < minY) minY = y
      if (x > maxX) maxX = x
      if (y > maxY) maxY = y

      ids(i) = i
    }

    val cx = (minX + maxX) / 2
    val cy = (minY + maxY) / 2

    var minDist = Double.PositiveInfinity

    var i0: Int = -1
    var i1: Int = -1
    var i2: Int = -1

    // pick a seed point close to the center
    for (i <- 0 until count) {
      val d = dist(cx, cy, coords(2 * i), coords(2 * i + 1))

      if (d < minDist) {
        i0 = i
        minDist = d
      }
    }

    val i0x = coords(2 * i0)
    val i0y = coords(2 * i0 + 1)

    minDist = Double.PositiveInfinity

    // Find the point closest to the seed
    for (i <- 0 until count) {
      if (i != i0) {
        val d = dist(i0x, i0y, coords(2 * i), coords(2 * i + 1))

        if (d < minDist && d > 0) {
          i1 = i
          minDist = d
        }
      }
    }

    var i1x = coords(2 * i1)
    var i1y = coords(2 * i1 + 1)

    var minRadius = Double.PositiveInfinity

    // Find the third point which forms the smallest circumcircle with the first two
    for (i <- 0 until count) {
      if (i != i0 && i != i1) {
        val r = circumradius(i0x, i0y, i1x, i1y, coords(2 * i), coords(2 * i + 1))

        if (r < minRadius) {
          i2 = i
          minRadius = r
        }
      }
    }

    if (minRadius == Double.PositiveInfinity) {
      // order collinear points by dx (or dy if all x are identical)
      // and return the list as a hull
      for (i <- 0 until count) {
        val a = coords(2 * i) - coords(0)
        val b = coords(2 * i + 1) - coords(1)
        dists(i) = if (a == 0.0) b else a
      }

      quicksort(ids, dists, 0, count - 1)

      val nhull = new Array[Int](count)
      var j = 0
      var d0 = Double.NegativeInfinity

      for (i <- 0 until count) {
        val id = ids(i)
        if (dists(id) > d0) {
          nhull(j) = id
          j += 1
          d0 = dists(id)
        }
      }

      hull = util.Arrays.copyOf(nhull, j)
      triangles = new Array[Int](0)
      halfedges = new Array[Int](0)

      return
    }

    var i2x = coords(2 * i2)
    var i2y = coords(2 * i2 + 1)

    // swap the order of the seed points for counter-clockwise orientation
    if (orient(i0x, i0y, i1x, i1y, i2x, i2y) < 0.0) {
      val i = i1
      val x = i1x
      val y = i1y
      i1 = i2
      i1x = i2x
      i1y = i2y
      i2 = i
      i2x = x
      i2y = y
    }


    val center = circumcenter(i0x, i0y, i1x, i1y, i2x, i2y)

    this.cx = center._1
    this.cy = center._2

    for (i <- 0 until count) {
      dists(i) = dist(coords(2 * i), coords(2 * i + 1), center._1, center._2)
    }

    // sort the points by distance from the seed triangle circumcenter
    quicksort(ids, dists, 0, count - 1)

    // set up the seed triangle as the starting hull
    hullStart = i0
    var hullSize = 3

    hullNext(i0) = i1
    hullNext(i1) = i2
    hullNext(i2) = i0

    hullPrev(i2) = i1
    hullPrev(i0) = i2
    hullPrev(i1) = i0

    hullTri(i0) = 0
    hullTri(i1) = 1
    hullTri(i2) = 2

    util.Arrays.fill(hullHash, -1)
    hullHash(hashKey(i0x, i0y)) = i0
    hullHash(hashKey(i1x, i1y)) = i1
    hullHash(hashKey(i2x, i2y)) = i2

    trianglesLen = 0
    addTriangle(i0, i1, i2, -1, -1, -1)

    var xp = 0.0
    var yp = 0.0

    for (k <- ids.indices) {
      breakable {
        val i = ids(k)
        val x = coords(2 * i)
        val y = coords(2 * i + 1)

        // skip near-duplicate points
        if (k > 0 && math.abs(x - xp) <= EPSILON && math.abs(y - yp) <= EPSILON) break

        xp = x
        yp = y

        // skip seed triangle points
        if (i == i0 || i == i1 || i == i2) break

        // find a visible edge on the convex hull using edge hash
        var start = 0
        val key = hashKey(x, y)

        breakable {
          for (j <- 0 until hashSize) {
            start = hullHash((key + j) % hashSize)

            if (start != -1 && start != hullNext(start)) break
          }
        }

        start = hullPrev(start)

        var e = start
        var q = hullNext(e)

        breakable {
          while (orient(x, y, coords(2 * e), coords(2 * e + 1), coords(2 * q), coords(2 * q + 1)) >= 0.0) {
            e = q

            if (e == start) {
              e = -1
              break
            }

            q = hullNext(e)
          }
        }

        if (e == -1) break // likely a near-duplicate point skip it

        // add the first triangle from the point
        var t = addTriangle(e, i, hullNext(e), -1, -1, hullTri(e))

        // recursively flip triangles from the point until they satisfy the Delaunay condition
        hullTri(i) = legalize(t + 2)
        hullTri(e) = t // keep track of boundary triangles on the hull
        hullSize += 1

        // walk forward through the hull, adding more triangles and flipping recursively
        var next = hullNext(e)
        q = hullNext(next)

        while (orient(x, y, coords(2 * next), coords(2 * next + 1), coords(2 * q), coords(2 * q + 1)) < 0.0) {
          t = addTriangle(next, i, q, hullTri(i), -1, hullTri(next))
          hullTri(i) = legalize(t + 2)
          hullNext(next) = next // mark as removed
          hullSize -= 1

          next = q
          q = hullNext(next)
        }

        // walk backward from the other side, adding more triangles and flipping
        if (e == start) {
          q = hullPrev(e)

          while (orient(x, y, coords(2 * q), coords(2 * q + 1), coords(2 * e), coords(2 * e + 1)) < 0.0) {
            t = addTriangle(q, i, e, -1, hullTri(e), hullTri(q))
            legalize(t + 2)
            hullTri(q) = t
            hullNext(e) = e // mark as removed
            hullSize -= 1

            e = q
            q = hullPrev(e)
          }
        }

        // update the hull indices
        hullStart = e
        hullPrev(i) = e

        hullNext(e) = i
        hullPrev(next) = i
        hullNext(i) = next

        // save the two new edges in the hash table
        hullHash(hashKey(x, y)) = i
        hullHash(hashKey(coords(2 * e), coords(2 * e + 1))) = e
      }
    }

    hull = new Array[Int](hullSize)

    var e = hullStart

    for (i <- 0 until hullSize) {
      hull(i) = e
      e = hullNext(e)
    }

    // trim typed triangle mesh arrays
    triangles = util.Arrays.copyOf(_triangles, trianglesLen)
    halfedges = util.Arrays.copyOf(_halfedges, trianglesLen)
  }

  private def legalize(a: Int): Int = {
    var i = 0
    var na = a
    var ar: Int = 0

    // recursion eliminated with a fixed-size stack
    breakable {
      while (true) {
        val b = _halfedges(na)

        /* if the pair of triangles doesn't satisfy the Delaunay condition
         * (p1 is inside the circumcircle of [p0, pl, pr]), flip them,
         * then do the same check/flip recursively for the new pair of triangles
         *
         *           pl                    pl
         *          /||\                  /  \
         *       al/ || \bl            al/    \a
         *        /  ||  \              /      \
         *       /  a||b  \    flip    /___ar___\
         *     p0\   ||   /p1   =>   p0\---bl---/p1
         *        \  ||  /              \      /
         *       ar\ || /br             b\    /br
         *          \||/                  \  /
         *           pr                    pr
         */
        val a0 = na - na % 3
        ar = a0 + (na + 2) % 3

        if (b == -1) { // convex hull edge
          if (i == 0) break
          i -= 1
          na = EDGE_STACK(i)
        } else {
          val b0 = b - b % 3
          val al = a0 + (na + 1) % 3
          val bl = b0 + (b + 2) % 3

          val p0 = _triangles(ar)
          val pr = _triangles(na)
          val pl = _triangles(al)
          val p1 = _triangles(bl)

          val illegal = inCircle(
            coords(2 * p0), coords(2 * p0 + 1),
            coords(2 * pr), coords(2 * pr + 1),
            coords(2 * pl), coords(2 * pl + 1),
            coords(2 * p1), coords(2 * p1 + 1))

          if (illegal) {
            _triangles(na) = p1
            _triangles(b) = p0

            val hbl = _halfedges(bl)

            // edge swapped on the other side of the hull (rare) fix the halfedge reference
            if (hbl == -1) {
              var e = hullStart
              do {
                if (hullTri(e) == bl) {
                  hullTri(e) = na
                  break
                }
                e = hullPrev(e)
              } while (e != hullStart)
            }
            link(na, hbl)
            link(b, _halfedges(ar))
            link(ar, bl)

            val br = b0 + (b + 1) % 3

            // don't worry about hitting the cap: it can only happen on extremely degenerate input
            if (i < EDGE_STACK.length) {
              EDGE_STACK(i) = br
              i += 1
            }
          } else {
            if (i == 0) break
            i -= 1
            na = EDGE_STACK(i)
          }
        }
      }
    }

    ar
  }

  private def link(a:Int, b:Int): Unit = {
    _halfedges(a) = b
    if (b != -1) _halfedges(b) = a
  }

  // add a new triangle given vertex indices and adjacent half-edge ids
  private def addTriangle(i0: Int, i1: Int, i2: Int, a: Int, b: Int, c: Int): Int = {
    val t = trianglesLen

    _triangles(t) = i0
    _triangles(t + 1) = i1
    _triangles(t + 2) = i2

    link(t, a)
    link(t + 1, b)
    link(t + 2, c)

    trianglesLen += 3

    t
  }

  private def hashKey(x: Double, y: Double): Int = {
    (math.floor(pseudoAngle(x - cx, y - cy) * hashSize) % hashSize).floor.toInt
  }
}

object Delaunator {
  val EPSILON: Double = math.pow(2.0, -52)
  val EDGE_STACK = new Array[Int](512)
  def circumradius(ax: Double, ay: Double, bx: Double, by: Double, cx: Double, cy: Double): Double = {
    val dx = bx - ax
    val dy = by - ay
    val ex = cx - ax
    val ey = cy - ay

    val bl = dx * dx + dy * dy
    val cl = ex * ex + ey * ey
    val d = 0.5 / (dx * ey - dy * ex)

    val x = (ey * bl - dy * cl) * d
    val y = (dx * cl - ex * bl) * d

    x * x + y * y
  }

  def circumcenter(ax: Double, ay: Double, bx: Double, by: Double, cx: Double, cy: Double): (Double, Double) = {
    val dx = bx - ax
    val dy = by - ay
    val ex = cx - ax
    val ey = cy - ay

    val bl = dx * dx + dy * dy
    val cl = ex * ex + ey * ey
    val d = 0.5 / (dx * ey - dy * ex)

    val x = ax + (ey * bl - dy * cl) * d
    val y = ay + (dx * cl - ex * bl) * d

    (x, y)
  }

  def quicksort(ids: Array[Int], dists: Array[Double], left: Int, right: Int): Unit = {
    if (right - left <= 20) {
      for (i <- (left + 1) to right) {
        val temp = ids(i)
        val tempDist = dists(temp)
        var j = i - 1
        while (j >= left && dists(ids(j)) > tempDist) {
          ids(j + 1) = ids(j)
          j -= 1
        }
        ids(j + 1) = temp
      }
    } else {
      val median = (left + right) / 2
      var i = left + 1
      var j = right

      swap(ids, median, i)

      if (dists(ids(left)) > dists(ids(right))) swap(ids, left, right)
      if (dists(ids(i)) > dists(ids(right))) swap(ids, i, right)
      if (dists(ids(left)) > dists(ids(i))) swap(ids, left, i)

      val temp = ids(i)
      val tempDist = dists(temp)

      breakable {
        while (true) {
          do i += 1 while (dists(ids(i)) < tempDist)
          do j -= 1 while (dists(ids(j)) > tempDist)
          if (j < i) break
          swap(ids, i, j)
        }
      }

      ids(left + 1) = ids(j)
      ids(j) = temp

      if (right - i + 1 >= j - left) {
        quicksort(ids, dists, i, right)
        quicksort(ids, dists, left, j - 1)
      } else {
        quicksort(ids, dists, left, j - 1)
        quicksort(ids, dists, i, right)
      }
    }
  }

  private def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }

  // return 2d orientation sign if we're confident in it through J. Shewchuk's error bound check
  private def orientIfSure(px: Double, py: Double, rx: Double, ry: Double, qx: Double, qy: Double): Double = {
    val l = (ry - py) * (qx - px)
    val r = (rx - px) * (qy - py)
    if (math.abs(l - r) >= (3.3306690738754716e-16 * math.abs(l + r))) l - r else 0.0
  }

  // a more robust orientation test that's stable in a given triangle (to fix robustness issues)
  private def orient(rx: Double, ry: Double, qx: Double, qy: Double, px: Double, py: Double): Double = {
    val a = orientIfSure(px, py, rx, ry, qx, qy)
    val b = orientIfSure(rx, ry, qx, qy, px, py)
    val c = orientIfSure(qx, qy, px, py, rx, ry)

    if (!isDoubleFalsy(a)) {
      a
    } else {
      if (!isDoubleFalsy(b)) {
        b
      } else {
        c
      }
    }
  }

  // monotonically increases with real angle, but doesn't need expensive trigonometry
  private def pseudoAngle(dx: Double, dy: Double): Double = {
    val p = dx / (math.abs(dx) + math.abs(dy))
    val a =  if (dy > 0.0) 3.0 - p else 1.0 + p

    a / 4.0 // [0..1]
  }

  private def inCircle(ax: Double, ay: Double, bx: Double, by: Double, cx: Double, cy: Double, px: Double, py: Double): Boolean = {
    val dx = ax - px
    val dy = ay - py
    val ex = bx - px
    val ey = by - py
    val fx = cx - px
    val fy = cy - py

    val ap = dx * dx + dy * dy
    val bp = ex * ex + ey * ey
    val cp = fx * fx + fy * fy

    dx * (ey * cp - bp * fy) -
    dy * (ex * cp - bp * fx) +
    ap * (ex * fy - ey * fx) < 0
  }

  private def dist(ax: Double, ay: Double, bx: Double, by: Double): Double = {
    val dx = ax - bx
    val dy = ay - by
    dx * dx + dy * dy
  }

  private def isDoubleFalsy(d: Double): Boolean = d == -0.0 || d == 0.0 || d.isNaN

  def fromPoints(points: Iterable[Vec2]): Delaunator = {
    val vs = new Array[Double](points.size * 2)
    var i = 0
    for (p <- points) {
      vs(i) = p.x
      vs(i + 1) = p.y
      i += 2
    }
    new Delaunator(vs)
  }
}
