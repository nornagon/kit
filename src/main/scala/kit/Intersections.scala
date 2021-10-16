package kit

object Intersections {
  sealed trait Intersection
  case class PointIntersection(p: Vec2) extends Intersection
  case class SegmentIntersection(s: Segment2) extends Intersection

  def intersections(a: Circle2, b: Circle2): Iterable[Intersection] = {
    val d2 = (a.c -> b.c).lengthSquared
    val rsum2 = (a.r + b.r) * (a.r + b.r)
    if (d2 > rsum2)
      return Seq.empty
    if (d2 == rsum2)
      return Seq(PointIntersection(a.c.lerp(b.c, math.sqrt(d2)/2)))
    ??? // >1 intersection
  }
  def intersects(a: Circle2, b: Circle2): Boolean = {
    val d2 = (a.c -> b.c).lengthSquared
    val rsum2 = (a.r + b.r) * (a.r + b.r)
    d2 <= rsum2
  }

  def intersections(a: Segment2, b: Segment2): Iterable[Intersection] = {
    // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
    val dir = a.a -> a.b
    val otherDir = b.a -> b.b
    val dxo = dir cross otherDir
    if (dxo == 0) {
      if (((a.a -> b.a) cross dir) == 0) {
        // The two segments are collinear.
        val t0 = ((a.a -> b.a) dot dir) / (dir dot dir)
        val t1 = t0 + ((otherDir dot dir) / (dir dot dir))
        if ((t1 > t0 && t0 <= 1 && t1 >= 0) || (t1 <= t0 && t1 <= 1 && t0 >= 0)) {
          // collinear and intersecting, return average of all points so `a intersection b` == `b intersection a`
          Seq(SegmentIntersection(Segment2(???, ???)))
        } else {
          // collinear but not intersecting
          Seq.empty
        }
      } else {
        // The two segments are parallel and non-intersecting.
        Seq.empty
      }
    } else {
      // If two segments are not collinear and meet at a common endpoint, return the common endpoint
      // exactly.
      if (a.a == b.a || a.a == b.b) return Seq(PointIntersection(a.a))
      if (a.b == b.a || a.b == b.b) return Seq(PointIntersection(a.b))
      val t = ((a.a -> b.a) cross otherDir) / dxo
      val u = ((a.a -> b.a) cross dir) / dxo
      if (0 <= t && t <= 1 && 0 <= u && u <= 1)
        Seq(PointIntersection(a.a + dir * t))
      else
        Seq.empty
    }
  }

  def intersects(a: Segment2, b: Segment2): Boolean = intersections(a, b).nonEmpty

  def intersects(a: AABB, b: AABB): Boolean =
    a.lower.x <= b.upper.x &&
      b.lower.x <= a.upper.x &&
      a.lower.y <= b.upper.y &&
      b.lower.y <= a.upper.y

  def intersects(a: Circle2, b: Segment2): Boolean = (b.closestPointTo(a.c) - a.c).length <= a.r

  def intersects(a: Circle2, b: AABB): Boolean = {
    val Circle2(c, r) = a
    val AABB(lower, upper) = b
    c.x + r >= lower.x && c.x - r <= upper.x && c.y + r >= lower.y && c.y - r <= upper.y
  }

  def intersects(a: Segment2, b: Polygon): Boolean = {
    b.segments.exists(intersects(a, _))
  }

  def intersects(a: Polygon, b: Polygon): Boolean =
    a.segments.exists(aSeg => b.segments.exists(bSeg => intersects(aSeg, bSeg)))

  def intersections(seg: Segment2, aabb: AABB): Iterable[Intersection] = aabb.segments.flatMap(s => intersections(s, seg))

  def intersections(c: Circle2, seg: Segment2): Iterable[Intersection] = {
    val da = seg.a - c.c
    val db = seg.b - c.c
    val r1 = c.r
    val r2 = 0 /* segment radius */
    val rsum = r1 + r2

    val qa = (da dot da) - 2.0f * (da dot db) + (db dot db)
    val qb = (da dot db) - (da dot da)
    val det = qb * qb - qa * ((da dot da) - rsum * rsum)

    if (det >= 0.0f) {
      val disc = math.sqrt(det)
      val t1 = (-qb - disc) / qa
      if (0.0f <= t1 && t1 <= 1.0f) {
        val n = da.lerp(db, t1).normed

        val point = seg.a.lerp(seg.b, t1) - n * r2
        return Some(PointIntersection(point))
      }
      val t2 = (-qb + disc) / qa
      if (0.0f <= t2 && t2 <= 1.0f) {
        val n = da.lerp(db, t2).normed

        val point = seg.a.lerp(seg.b, t2) - n * r2
        return Some(PointIntersection(point))
      }
    }
    None
  }
}
