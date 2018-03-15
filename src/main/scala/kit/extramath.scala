package kit

object extramath {
  import scala.math._

  // https://medium.com/@nornagon/math-for-game-developers-parameterised-easing-9336a50c816d
  // -1 <= t <= 1
  def ease(k: Double)(t: Double): Double =
    if (t < -1) -1
    else if (t < 0) pow(1 + t, k) - 1
    else if (t < 1) 1 - pow(1 - t, k)
    else 1

  def ease01(k: Double)(t: Double): Double = (ease(k)(t * 2 - 1) + 1) / 2
}
