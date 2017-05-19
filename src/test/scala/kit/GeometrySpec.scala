package kit

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GeometrySpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSize = 100, maxSize = 200)

  property("forAngle equivalent to Mat33.rotate *") {
    forAll { (a: Double) =>
      Vec2.forAngle(a) should be (Mat33.rotate(a) * Vec2(1, 0))
    }
  }

  property("forAngle inverse of toAngle within [-pi,pi]") {
    forAll(Gen.choose(-math.Pi, math.Pi)) { (a: Double) =>
      Vec2.forAngle(a).toAngle shouldBe a +- 1e-10
    }
  }

  property("Angle.clipToPi within [-pi,pi]") {
    forAll(Gen.choose(-1e10, 1e10)) { (a: Double) =>
      val clipped = Angle.clipToPi(a)
      clipped should (be >= -math.Pi and be <= math.Pi)
    }
  }
}
