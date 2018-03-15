package kit.pcg

import kit.{Circle2, Vec2}


class Noise(seed: Int) {
  case class Grad(x: Double, y: Double, z: Double) {
    def dot2(ox: Double, oy: Double): Double = ox*x + oy*y
    def dot3(ox: Double, oy: Double, oz: Double): Double = ox*x + oy*y + oz*z
  }

  private val grad3 = Seq(
    Grad(1, 1, 0), Grad(-1, 1, 0), Grad(1, -1, 0), Grad(-1, -1, 0),
    Grad(1, 0, 1), Grad(-1, 0, 1), Grad(1, 0, -1), Grad(-1, 0, -1),
    Grad(0, 1, 1), Grad(0, -1, 1), Grad(0, 1, -1), Grad(0, -1, -1)
  )
  private val grads = grad3//Circle2(Vec2(0, 0), 1).toPolygon(12).points.map { case Vec2(x, y) => Grad(x, y, 0) }
  private val p = Seq(
    151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140,
    36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120,
    234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33,
    88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71,
    134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133,
    230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161,
    1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130,
    116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250,
    124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227,
    47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44,
    154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253, 19,
    98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228,
    251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235,
    249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176,
    115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29,
    24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180
  )
  private val perm = new Array[Int](512)
  private val gradP = new Array[Grad](512)

  private def generateSeedData(_seed: Int): Unit = {
    val seed = if (_seed < 256) _seed | (_seed << 8) else _seed

    for (i <- 0 until 256) {
      val v =
        if ((i & 1) != 0) {
          p(i) ^ (seed & 255)
        } else {
          p(i) ^ ((seed >> 8) & 255)
        }

      perm(i) = v
      perm(i + 256) = v
      gradP(i) = grads(v % 12)
      gradP(i + 256) = grads(v % 12)
    }
  }

  generateSeedData(seed)

  // Skewing and unskewing factors
  private def F(n: Int): Double = (Math.sqrt(n + 1.0) - 1) / n
  private def G(n: Int): Double = -(1/Math.sqrt(n + 1.0) - 1) / n
  private val F2 = F(2)
  private val G2 = G(2)
  private val F3 = F(3) // = 1/3
  private val G3 = G(3) // = 1/6

  // 2D simplex noise
  def simplex2(xin: Double, yin: Double): Double = {
    // Skew the input space to determine which simplex cell we're in
    val s = (xin + yin) * F2 // Hairy factor for 2D
    val i: Int = Math.floor(xin + s).toInt
    val j: Int = Math.floor(yin + s).toInt
    val t = (i + j) * G2
    val x0 = xin - i + t // The x,y distances from the cell origin, unskewed.
    val y0 = yin - j + t
    // For the 2D case, the simplex shape is an equilateral triangle.
    // Determine which simplex we are in.
    // Offsets for second (middle) corner of simplex in (i,j) coords
    val (i1, j1) =
      if (x0 > y0) (1, 0) // lower triangle, XY order: (0,0)->(1,0)->(1,1)
      else (0, 1) // upper triangle, YX order: (0,0)->(0,1)->(1,1)
    // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
    // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
    // c = (3-sqrt(3))/6
    val x1 = x0 - i1 + G2 // Offsets for middle corner in (x,y) unskewed coords
    val y1 = y0 - j1 + G2
    val x2 = x0 - 1 + 2 * G2 // Offsets for last corner in (x,y) unskewed coords
    val y2 = y0 - 1 + 2 * G2
    // Work out the hashed gradient indices of the three simplex corners
    val ii = i & 255
    val jj = j & 255
    val gi0 = gradP(ii + perm(jj))
    val gi1 = gradP(ii + i1 + perm(jj + j1))
    val gi2 = gradP(ii + 1 + perm(jj + 1))
    // n0, n1, n2 are the contributions from the three corners
    var t0 = 0.5 - x0 * x0 - y0 * y0
    val n0 = if (t0 < 0) {
      0
    } else {
      t0 *= t0
      t0 * t0 * gi0.dot2(x0, y0)  // (x,y) of grad3 used for 2D gradient
    }
    var t1 = 0.5 - x1 * x1 - y1 * y1
    val n1 = if (t1 < 0) {
      0
    } else {
      t1 *= t1
      t1 * t1 * gi1.dot2(x1, y1)
    }
    var t2 = 0.5 - x2 * x2 - y2 * y2
    val n2 = if (t2 < 0) {
      0
    } else {
      t2 *= t2
      t2 * t2 * gi2.dot2(x2, y2)
    }
    // Add contributions from each corner to get the final noise value.
    // The result is scaled to return values in the interval [-1,1].
    70 * (n0 + n1 + n2)
  }

  def simplex3(xin: Double, yin: Double, zin: Double): Double = {
    // Skew the input space to determine which simplex cell we're in
    val s = (xin + yin + zin) * F3 // Hairy factor for 3D
    val i: Int = Math.floor(xin + s).toInt
    val j: Int = Math.floor(yin + s).toInt
    val k: Int = Math.floor(zin + s).toInt

    val t = (i + j + k) * G3
    // The x,y distances from the cell origin, unskewed.
    val x0 = xin - i + t
    val y0 = yin - j + t
    val z0 = zin - k + t

    // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
    // Determine which simplex we are in.
    val (i1, j1, k1, i2, j2, k2) =
      if (x0 >= y0) {
        if (y0 >= z0)      (1, 0, 0, 1, 1, 0)
        else if (x0 >= z0) (1, 0, 0, 1, 0, 1)
        else               (0, 0, 1, 1, 0, 1)
      } else {
        if (y0 < z0)       (0, 0, 1, 0, 1, 1)
        else if (x0 < z0)  (0, 1, 0, 0, 1, 1)
        else               (0, 1, 0, 1, 1, 0)
      }
    // A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
    // a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
    // a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
    // c = 1/6.

    // Offsets for second corner
    val x1 = x0 - i1 + G3
    val y1 = y0 - j1 + G3
    val z1 = z0 - k1 + G3

    // Offsets for third corner
    val x2 = x0 - i2 + G3 * 2
    val y2 = y0 - j2 + G3 * 2
    val z2 = z0 - k2 + G3 * 2

    // Offsets for fourth corner
    val x3 = x0 - 1 + G3 * 3
    val y3 = y0 - 1 + G3 * 3
    val z3 = z0 - 1 + G3 * 3

    // Work out the hashed gradient indices of the four simplex corners
    val ii = i & 255
    val jj = j & 255
    val kk = k & 255
    val gi0 = gradP(ii + perm(jj + perm(kk)))
    val gi1 = gradP(ii + i1 + perm(jj + j1 + perm(kk + k1)))
    val gi2 = gradP(ii + i2 + perm(jj + j2 + perm(kk + k2)))
    val gi3 = gradP(ii + 1 + perm(jj + 1 + perm(kk + 1)))

    // Calculate the contribution from the four corners
    val t0 = 0.6 - x0*x0 - y0*y0 - z0*z0
    val n0 =
      if (t0 < 0) 0
      else {
        val t0_2 = t0 * t0
        t0_2 * t0_2 * gi0.dot3(x0, y0, z0) // (x,y) of grad3 used for 2D gradient
      }
    val t1 = 0.6 - x1*x1 - y1*y1 - z1*z1
    val n1 =
      if (t1 < 0) 0
      else {
        val t1_2 = t1 * t1
        t1_2 * t1_2 * gi1.dot3(x1, y1, z1)
      }
    val t2 = 0.6 - x2*x2 - y2*y2 - z2*z2
    val n2 =
      if (t2 < 0) 0
      else {
        val t2_2 = t2 * t2
        t2_2 * t2_2 * gi2.dot3(x2, y2, z2)
      }
    val t3 = 0.6 - x3*x3 - y3*y3 - z3*z3
    val n3 =
      if (t3 < 0) 0
      else {
        val t3_2 = t3 * t3
        t3_2 * t3_2 * gi3.dot3(x3, y3, z3)
      }
    // Add contributions from each corner to get the final noise value.
    // The result is scaled to return values in the interval [-1,1].
    32 * (n0 + n1 + n2 + n3)
  }
}
