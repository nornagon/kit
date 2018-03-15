package kit.pcg

import kit.Vec2

class LayeredNoise(noises: Seq[(Double, Double) => Double]) {
  def at(v: Vec2): Double = at(v.x, v.y)
  def at(x: Double, y: Double): Double = {
    val d = (0d /: noises) {
      _ + _ (x, y)
    }
    assert(d >= -1 && d <= 1, "bad noise")
    d
  }
}

object LayeredNoise {
  def octaves(n: Int)(implicit r: scala.util.Random): LayeredNoise = {
    new LayeredNoise(for (o <- 1 to n) yield {
      val noise = new Noise(r.nextInt())
      val s = math.pow(2, o)
      (x: Double, y: Double) => noise.simplex2(x * s, y * s) / n
    })
  }
}
