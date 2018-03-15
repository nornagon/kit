package kit

import scala.collection.mutable

object FOV {
  case class Endpoint(p: Vec2, angle: Double, segment: Segment2, isBegin: Boolean)

  /** True if `point` is "to the left" of the line collinear with `segment`.
    *        b
    * left  /   x
    *      /
    *     a   right
    */
  def isLeftOf(segment: Segment2, point: Vec2): Boolean =
    ((segment.a -> segment.b) cross (segment.a -> point)) < 0

  /** True if `b` is "closer to" `relativeTo` than `a`.
    * See http://www.redblobgames.com/articles/visibility/segment-sorting.html
    */
  def segmentInFrontOf(a: Segment2, b: Segment2, relativeTo: Vec2): Boolean = {
    val A1 = isLeftOf(a, b.a.lerp(b.b, 0.01))
    val A2 = isLeftOf(a, b.b.lerp(b.a, 0.01))
    val A3 = isLeftOf(a, relativeTo)
    val B1 = isLeftOf(b, a.a.lerp(a.b, 0.01))
    val B2 = isLeftOf(b, a.b.lerp(a.a, 0.01))
    val B3 = isLeftOf(b, relativeTo)

    (B1 == B2 && B2 != B3) || (A1 == A2 && A2 == A3)
  }

  def lineIntersection(s1: Segment2, s2: Segment2): Vec2 = {
    val s2v = s2.a -> s2.b
    val s = (s2v cross (s1.a - s2.a)) / ((s1.b - s1.a) cross s2v)
    s1.a + (s1.a -> s1.b) * s
  }

  def getTrianglePoints(source: Vec2, a1: Double, a2: Double, segment: Option[Segment2]): Seq[Vec2] = {
    val v1 = Vec2.forAngle(a1)
    val v2 = Vec2.forAngle(a2)
    val seg = segment match {
      case Some(s: Segment2) => s
      case None => Segment2(source + v1 * 2000, source + v2 * 2000)
    }

    Seq(
      lineIntersection(seg, Segment2(source, source + v1)),
      lineIntersection(seg, Segment2(source, source + v2))
    )
  }

  /** Amit Patel's algorithm for computing field of view.
    *
    * TODO: it'd be fairly easy to add segment normals to this, to cull backfaces
    * and allow for some other fancy tricks :)
    *
    * @see http://www.redblobgames.com/articles/visibility/
    * @param source The source of the "vision"
    * @param segments A list of segments which block vision.
    * @param bounds A bounding box beyond which vision will not propagate.
    * @return A list of vertices which form the edge of vision.
    */
  def calculateFOV(source: Vec2, segments: Seq[Segment2], bounds: AABB): Seq[Vec2] = {
    val endpoints = (segments ++ bounds.toPolygon.segments).flatMap { segment =>
      bounds.truncate(segment) match {
        case None =>
          Seq.empty
        case Some(truncated) =>
          val aAngle = (source -> segment.a).toAngle
          val bAngle = (source -> segment.b).toAngle
          var dAngle = bAngle - aAngle
          if (dAngle <= -Math.PI) dAngle += 2 * Math.PI
          if (dAngle > Math.PI) dAngle -= 2 * Math.PI
          Seq(
            Endpoint(segment.a, aAngle, truncated, dAngle > 0),
            Endpoint(segment.b, bAngle, truncated, dAngle <= 0)
          )
      }
    }.sortWith((a, b) => a.angle < b.angle || (a.angle == b.angle && a.isBegin && !b.isBegin))

    // TODO: This data structure could definitely be more efficient. SortedSet doesn't work unfortunately because
    // segmentInFrontOf is not a total ordering.
    var openSegments = List.empty[Segment2]
    def addSeg(seg: Segment2): Unit = {
      val (closer, further) = openSegments.span(s => segmentInFrontOf(seg, s, source))
      openSegments = closer ++ (seg :: further)
    }
    def delSeg(seg: Segment2): Unit = {
      val (before, after) = openSegments.span(_ ne seg)
      openSegments = before ++ after.drop(1)
    }
    var beginAngle: Double = 0
    // The first pass is just to set up `beginAngle` and `openSegments` correctly. TODO: do this more efficiently.
    for (endpoint <- endpoints) {
      val previouslyClosestSegment = openSegments.headOption
      if (endpoint.isBegin)
        addSeg(endpoint.segment)
      else
        delSeg(endpoint.segment)
      if (previouslyClosestSegment != openSegments.headOption)
        beginAngle = endpoint.angle
    }
    val output = mutable.Buffer[Seq[Vec2]]()
    for (endpoint <- endpoints) {
      val previouslyClosestSegment = openSegments.headOption
      if (endpoint.isBegin)
        addSeg(endpoint.segment)
      else
        delSeg(endpoint.segment)
      if (previouslyClosestSegment != openSegments.headOption) {
        output.append(getTrianglePoints(source, beginAngle, endpoint.angle, previouslyClosestSegment))
        beginAngle = endpoint.angle
      }
    }
    output.flatten
  }

  /** Returns the point on `ray` that is also in `other`, or 1.0 if no such point exists */
  def rayHit(ray: Segment2, other: Segment2): Double = {
    // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
    val dir = ray.a -> ray.b
    val otherDir = other.a -> other.b
    val dxo = dir cross otherDir
    if (dxo == 0) {
      if (((ray.a -> other.a) cross dir) == 0) {
        // The two segments are collinear.
        val t0 = ((ray.a -> other.a) dot dir) / (dir dot dir)
        val t1 = t0 + ((otherDir dot dir) / (dir dot dir))
        if ((t1 > t0 && t0 <= 1 && t1 >= 0) || (t1 <= t0 && t1 <= 1 && t0 >= 0))
          // TODO hmm
          Math.min(t0, t1)
        else
        // collinear but not intersecting
          1.0
      } else {
        // The two segments are parallel and non-intersecting.
        1.0
      }
    } else {
      val t = ((ray.a -> other.a) cross otherDir) / dxo
      val u = ((ray.a -> other.a) cross dir) / dxo
      //println(s"non-collinear, $ray $other $t $u")
      if (0 <= t && t <= 1 && 0 <= u && u <= 1)
        t
      else
        1.0
    }
  }

  /** Raycasting approach to computing FOV.
    *
    * Heavily based on https://legends2k.github.io/2d-fov/design.html
    *
    * TODO: currently, the algorithm expects `segments` to be non-intersecting, and will produce weird artifacts if they
    * are not. This can be solved by simply breaking all the segments into pieces wherever they overlap, e.g. by
    * Bentley-Ottman or Balaban95.
    *
    * Also, this seems to be about 10x slower than calculateFOV().
    *
    * @param source The source of the "vision"
    * @param segments A list of segments which block vision.
    * @param bounds A bounding box beyond which vision will not propagate.
    * @return A list of vertices which form the edge of vision.
    */
  def calculateFOV2(source: Vec2, segments: Seq[Segment2], bounds: AABB): Seq[Vec2] = {
    if (segments.isEmpty)
      return bounds.corners
    // TODO: this should automatically truncate the segments to the bounds
    val maxDist = (bounds.upper - bounds.lower).length / 2
    val points = mutable.Set.empty[Vec2] ++ bounds.toPolygon.points
    val segmentsAtPoint = mutable.Map.empty[Vec2, Seq[Vec2]].withDefaultValue(Seq.empty)
    for (seg <- segments) {
      points += seg.a
      points += seg.b
      segmentsAtPoint(seg.a) :+= seg.b
      segmentsAtPoint(seg.b) :+= seg.a
    }
    val outline = points.toSeq.flatMap { p =>
      val dir = (source -> p).normed
      val connectedPoints = segmentsAtPoint(p)
      val ray = Segment2(source, p)
      val adjustedRay =
        if (connectedPoints.forall(isLeftOf(ray, _)))
          Segment2(source, source + dir.rotate(-0.0001) * maxDist)
        else if (connectedPoints.forall(!isLeftOf(ray, _)))
          Segment2(source, source + dir.rotate(0.0001) * maxDist)
        else
          ray
      for (r <- Set(ray, adjustedRay)) yield {
        val minT = segments.view.map(rayHit(r, _)).min
        r.a.lerp(r.b, minT)
      }
    }.sortBy(p => (source -> p).toAngle)
    outline :+ outline.head
  }
}
