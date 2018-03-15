package kit

object RandomImplicits {
  implicit class ExtendedRandom(r: scala.util.Random) {
    /** Returns an angle in the range [0, 2pi) */
    def angle: Double = r.nextDouble() * 2 * Math.PI
    /** Returns a double in the range [a, b) */
    def between(a: Double, b: Double): Double = r.nextDouble() * (b - a) + a
    /** Returns an integer in the range [a, b) */
    def between(a: Int, b: Int): Int = (r.nextDouble() * (b - a) + a).floor.toInt
    /** Returns true one time out of n (on average) */
    def oneIn(n: Int): Boolean = between(0, n) == 0

    def bilateral: Double = r.nextDouble() * 2 - 1
    def bilateral(k: Double): Double = k * (r.nextDouble() * 2 - 1)

    /** A point uniformly sampled from the origin-centered circle with the given `radius`.
      * http://stackoverflow.com/questions/5837572/generate-a-random-point-within-a-circle-uniformly
      */
    def withinCircle(radius: Double = 1): Vec2 = withinAnnulus(0, radius)
    def withinAnnulus(r0: Double, r1: Double): Vec2 = {
      val u = r.nextDouble() + r.nextDouble() match { case x if x > 1 => 2 - x; case x => x }
      Vec2.forAngle(angle) * (u * (r1 - r0) + r0)
    }
    def withinAABB(bounds: AABB): Vec2 =
      Vec2(
        between(bounds.lower.x, bounds.upper.x),
        between(bounds.lower.y, bounds.upper.y)
      )

    def oneOf[K](options: K*): K = pick(options)
    def nOf[K](n: Int, options: Seq[K]): Seq[K] = r.shuffle(options).take(n)
    def pick[K](options: Seq[K]): K = options(between(0, options.size).floor.toInt)
    def pick[K](options: Iterable[K]): K = {
      var chosen = options.head
      var i = 1
      for (o <- options.tail) {
        if (oneIn(i + 1))
          chosen = o
        i += 1
      }
      chosen
    }

    // https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_A-Res
    def chooseFrom[T](elems: TraversableOnce[T])(weight: T => Double): T =
      elems.maxBy { elem => math.pow(r.nextDouble(), 1 / weight(elem)) }
  }
}

object Rand extends RandomImplicits.ExtendedRandom(scala.util.Random)
