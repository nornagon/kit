package kit

import scala.collection.mutable

/**
  * "An optimal algorithm for finding segments intersections", by Ivan J. Balaban.
  */
object Balaban95 {
  /** True iff s spans the strip (b, e) */
  private def spansStrip(s: Segment2, b: Double, e: Double): Boolean =
    s.left.x <= b && e <= s.right.x

  /** True iff segments p and q intersect between x=b and x=e */
  private def intersectsInsideStrip(p: Segment2, q: Segment2, b: Double, e: Double): Boolean = {
    for (i <- Intersections.intersections(p, q)) {
      i match {
        case Intersections.PointIntersection(pi) =>
          if (pi.x >= b && pi.x <= e)
            return true
        case Intersections.SegmentIntersection(s) => ???
      }
    }
    false
  }

  /** Identifies an endpoint as being either the left (beginning) or the right (end) of a segment. */
  private sealed trait SegmentSide
  private case object Left extends SegmentSide
  private case object Right extends SegmentSide

  /** Reports an intersection between two segments. */
  private type Intersection[T <: Segment2] = (T, T, Intersections.Intersection)

  /**
    * @param leftSegs segments intersecting the line x=b, ordered by y coordinate of intersection
    * @param b x-coordinate of beginning of strip
    * @param e x-coordinate of end of strip
    * @return (staircase, rest)
    */
  private def split[T <: Segment2](leftSegs: Seq[T], b: Double, e: Double): (Seq[T], Seq[T]) = {
    //println(s".split $leftSegs $b $e")
    //assert(isSorted(leftSegs.map(s => s.yAtX(b))), s"$leftSegs at $b weren't sorted: ${leftSegs.map(s => s.yAtX(b))}")
    val staircase = mutable.Buffer.empty[T]
    val rest = mutable.Buffer.empty[T]
    for (s <- leftSegs) {
      (
        if (spansStrip(s, b, e) && (staircase.isEmpty || !intersectsInsideStrip(s, staircase.last, b, e)))
          staircase
        else
          rest
        ).append(s)
    }
    //println(s".   => $staircase  ///  $rest")
    (staircase, rest)
  }

  /**
    * Given a set of segments `L` sorted by their y values at `b`, all of which span the strip
    * (b,e), find all intersections between the segments and return the segments sorted by their
    * y value at `e`.
    */
  private def searchInStrip[T <: Segment2](LwithVerts: Seq[T], b: Double, e: Double): (Seq[T], Seq[Intersection[T]]) = {
    val L = LwithVerts.filter(s => s.a.x != s.b.x)
    //println(s"Searching in strip $L $b $e")
    //assert(L.forall(s => spansStrip(s, b, e)))
    val (staircase, rest) = split(L, b, e)
    //println(s"split to $staircase, $rest")
    if (rest.isEmpty) {
      (staircase, Seq.empty)
    } else {
      val ix = findStaircaseIntersections(staircase, rest, b, e, b)
      val (r, ixs) = searchInStrip(rest, b, e)
      (merge(staircase, r, e), ix ++ ixs)
    }
  }

  /**
    * Merge two sorted sequences of segments according to their y values at x.
    * Both sequences must each be sorted within themselves by their y values at x.
    * Segments in both sequences are assumed to be defined at x.
    */
  private def merge[T <: Segment2](as: Seq[T], bs: Seq[T], x: Double): Seq[T] = {
    //assert(isSorted(as.map(_.yAtX(x))))
    //assert(isSorted(bs.map(_.yAtX(x))))

    if (as.isEmpty)
      return bs
    if (bs.isEmpty)
      return as

    val ret =
      if (as.head.yAtX(x) < bs.head.yAtX(x)) {
        as.head +: merge(as.tail, bs, x)
      } else {
        bs.head +: merge(as, bs.tail, x)
      }

    //assert(ret.size == as.size + bs.size)
    //assert(isSorted(ret.map(_.yAtX(x))), s"Merge is wrong: merged $as $bs to $ret")
    ret
  }

  /**
    * Given a staircase, a segment, and the segment's position in the staircase `loc(q, s)`,
    * return the set of intersections between the staircase and the segment in O(1 + |Int(q,s)|).
    */
  private def staircaseIntersections[T <: Segment2](staircase: Seq[T], i: Int, seg: T, b: Double, e: Double): Seq[Intersection[T]] = {
    val downIntersections =
      staircase.view(i, staircase.size) map { s => (seg, s, Intersections.intersections(s, seg).headOption) } takeWhile (_._3.isDefined)
    val upIntersections =
      staircase.view(0, i).reverse map { s => (seg, s, Intersections.intersections(s, seg).headOption) } takeWhile (_._3.isDefined)
    (downIntersections ++ upIntersections).map {
      case (s1, s2, Some(ix)) => (s1, s2, ix)
    }
  }

  /**
    * Find all the intersections between `staircase` and `others`, given that both lists are sorted
    * by their y values at `x`.
    * (?)
    */
  private def findStaircaseIntersections[T <: Segment2](staircase: Seq[T], others: Seq[T], b: Double, e: Double, x: Double): Seq[Intersection[T]] = {
    if (others.isEmpty)
      return Seq.empty
    val ret = mutable.Buffer.empty[Intersection[T]]
    val sIter = others.iterator
    var curS = sIter.next()
    for ((stair, i) <- staircase.zipWithIndex) {
      val x1 = stair.yAtX(x)
      while (x1 >= curS.yAtX(x)) {
        ret ++= staircaseIntersections(staircase, i, curS, b, e)
        if (!sIter.hasNext)
          return ret
        curS = sIter.next()
      }
    }
    ret ++= staircaseIntersections(staircase, staircase.size, curS, b, e)
    sIter foreach { s =>
      ret ++= staircaseIntersections(staircase, staircase.size, s, b, e)
    }
    ret
  }

  /**
    * Finds intersections between `staircase` and `unsorted` between `b` and `e`.
    * Unlike findStaircaseIntersections, doesn't require that `unsorted` be sorted.
    */
  private def findUnsortedIntersections[T <: Segment2](staircase: Seq[T], unsorted: Seq[T], b: Double, e: Double): Seq[Intersection[T]] = {
    unsorted flatMap { s =>
      val i = loc(staircase, s, b, e)
      staircaseIntersections(staircase, i, s, b, e)
    }
  }

  /**
    * Determine the location of s in staircase, i.e. an index i into staircase s.t. between
    * staircase(i) and staircase(i+1) lies at least one point on s.
    *
    * Assumes that s crosses the strip ⟨b,e⟩.
    */
  private def loc[T <: Segment2](staircase: Seq[T], s: T, b: Double, e: Double): Int = {
    val x = math.max(s.left.x, b)
    val y1 = s.yAtX(x)
    var (start, finish) = (0, staircase.size)
    while (start != finish) {
      val center = (start + finish) / 2
      if (y1 < staircase(center).yAtX(x))
        finish = center
      else
        start = center + 1
    }
    start
  }

  private def isSorted(l: Iterable[Double]): Boolean =
    l.sliding(2).forall { case Seq() | Seq(_) => true; case Seq(a, b) => a <= b }

  /**
    * Divide and conquer to find all the intersections between `b` and `e`.
    *
    * @param endpoints Complete endpoints structure, will be indexed by values between `b` and `e`.
    * @param Lv Segments that cross the line x=b, sorted by their y values at x=b.
    * @param Iv Segments that lie entirely between x=b and x=e, unsorted.
    * @param b Index of the leftmost endpoint of the strip under consideration.
    * @param e Index of the rightmost endpoint of the strip under consideration.
    * @return The segments that cross the line x=e, sorted by their y values at x=e, and any
    *         intersections found in the process.
    */
  private def treeSearch[T <: Segment2](endpoints: Endpoints[T], Lv: Seq[T], Iv: Seq[T], b: Int, e: Int): (Seq[T], Seq[Intersection[T]]) = {
    //println(s"treeSearch $Lv $Iv $b $e")
    val bx = endpoints.x(b)
    val ex = endpoints.x(e)
    //assert(isSorted(Lv.map(s => s.yAtX(bx))), s"$Lv at $bx weren't sorted: ${Lv.map(_.yAtX(bx))}")
    if (e - b == 1) {
      return searchInStrip(Lv, bx, ex)
    }
    val (q, lls) = split(Lv, bx, ex)
    //println(s"Split in tS to $q $lls")
    val c = (b + e) / 2
    val cx = endpoints.x(c)
    val ils = Iv.filter(seg => seg.left.x > bx && seg.right.x < cx)
    val irs = Iv.filter(seg => seg.left.x > cx && seg.right.x < ex)
    val (rls, ixs) = treeSearch(endpoints, lls, ils, b, c)
    val lrs = mutable.Buffer.empty[T] ++ rls
    endpoints.events(c) foreach {
      case (seg, Left) => // seg appears
        val pos = loc(lrs, seg, cx, ex)
        lrs.insert(pos, seg)
      //assert(isSorted(lrs.map(_.yAtX(cx))), s"Bad seg appear $pos \n${lrs.map(_.yAtX(cx))}")
      case (seg, Right) => // seg goes away
        val pos = lrs.indexOf(seg)
        if (pos >= 0)
          lrs.remove(pos)
      //assert(isSorted(lrs.map(_.yAtX(cx))), s"Bad seg disappear $pos")
    }
    val (rrs, ixs2) = treeSearch(endpoints, lrs, irs, c, e)

    val ixL = findStaircaseIntersections(q, lls, bx, ex, bx)
    val ixI = findUnsortedIntersections(q, Iv, bx, ex)
    val ixR = findStaircaseIntersections(q, rrs, bx, ex, ex)

    val merged = merge(q, rrs, ex)
    //assert(isSorted(merged.map(s => s.yAtX(ex))), s"merged $merged at end $ex weren't sorted: ${merged.map(_.yAtX(ex))}")

    (merged, ixL ++ ixs ++ ixI ++ ixs2 ++ ixR)
  }

  /**
    * Structure to keep track of endpoint "events", in sorted order.
    *
    * @param endpointXs x values of endpoints of the segment set, in increasing order.
    * @param endpointSegs For each x value, a list of "events" (segment start or end) that occur.
    */
  private case class Endpoints[T <: Segment2](
    endpointXs: Seq[Double],
    endpointSegs: Map[Double, Seq[(T, SegmentSide)]]
  ) {
    require(endpointXs.size == endpointSegs.size)
    def size: Int = endpointXs.size

    def x(i: Int): Double = endpointXs(i)
    def events(i: Int): Seq[(T, SegmentSide)] = endpointSegs(x(i))
  }

  /** Zhu Li, do the thing! */
  def intersectingPairs[T <: Segment2](ss: Seq[T]): Seq[Intersection[T]] = {
    if (ss.size <= 1) return Seq.empty
    val m = mutable.Map.empty[Double, Seq[(T, SegmentSide)]]
    for (s <- ss) {
      for (ep <- Seq((s.left, Left), (s.right, Right))) {
        if (!m.contains(ep._1.x))
          m.put(ep._1.x, Seq((s, ep._2)))
        else
          m(ep._1.x) :+= (s, ep._2)
      }
    }
    val seq = m.keys.toSeq.sorted
    val endpoints = Endpoints(seq, m.toMap)
    val left = endpoints.endpointXs.head
    val right = endpoints.endpointXs.last
    val Lr = endpoints.endpointSegs(left).map(_._1).sortBy(_.yAtX(left))
    val Ir = ss.filter(s => s.a.x > left && s.a.x < right && s.b.x > left && s.b.x < right)
    val (_, ixs) = treeSearch(endpoints, Lr, Ir, 0, endpoints.size - 1)
    ixs
  }

  /** For comparison :) */
  def naiveIntersectingPairs[T <: Segment2](ss: Seq[T]): Seq[Intersection[T]] = {
    for {
      ai <- ss.indices
      bi <- (ai+1) until ss.size
      a = ss(ai)
      b = ss(bi)
      if a.a != b.a && a.a != b.b && a.b != b.a && a.b != b.b
      ix <- Intersections.intersections(a, b)
    } yield (a, b, ix)
  }
}
