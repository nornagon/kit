package kit

object RandomImplicits {
  implicit class ExtendedRandom(r: scala.util.Random) {
    /** Returns an angle in the range [0, 2pi) */
    def angle = r.nextDouble() * 2 * Math.PI
    /** Returns a double in the range [a, b) */
    def between(a: Double, b: Double): Double = r.nextDouble() * (b - a) + a
    /** Returns an integer in the range [a, b) */
    def between(a: Int, b: Int): Int = (r.nextDouble() * (b - a) + a).floor.toInt
    /** Returns true one time out of n (on average) */
    def oneIn(n: Int): Boolean = between(0, n) == 0

    /** A point uniformly sampled from the origin-centered circle with the given `radius`.
      * http://stackoverflow.com/questions/5837572/generate-a-random-point-within-a-circle-uniformly
      */
    def withinCircle(radius: Double = 1): Vec2 = {
      val u = r.nextDouble() + r.nextDouble() match { case x if x > 1 => 2 - x; case x => x }
      Vec2.forAngle(angle) * u * radius
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

    type Distribution[K] = Map[K, Double]

    def chooseFrom[K](distribution: (K, Double)*): K = chooseFrom(Map(distribution: _*))
    def chooseFrom[K](distribution: Distribution[K]): K = {
      val norm = distribution.values.sum
      val x = r.nextDouble * norm
      var sum = 0d
      for ((t, p) <- distribution) {
        sum += p
        if (x < sum)
          return t
      }
      throw new RuntimeException(s"chooseFrom is wrong or $distribution is not a distribution")
    }
  }
}

object Rand extends RandomImplicits.ExtendedRandom(scala.util.Random)
