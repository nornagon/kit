package kit


object Angle {
  /** Returns an angle equivalent to `a` but within the range [-π, π] */
  def clipToPi(a: Double): Double = {
    if (a < -math.Pi)
      a + (math.Pi * 2 * ((a + math.Pi) / (math.Pi * 2)).floor.abs)
    else if (a > math.Pi)
      a - (math.Pi * 2 * ((a - math.Pi) / (math.Pi * 2)).ceil.abs)
    else
      a
  }
}

case class Vec2(x: Double, y: Double) {
  def +(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
  def -(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
  def *(k: Double): Vec2 = Vec2(x * k, y * k)
  def /(k: Double): Vec2 = Vec2(x / k, y / k)
  def unary_-(): Vec2 = Vec2(-x, -y)
  /** Hadamard product */
  def o(other: Vec2): Vec2 = Vec2(x * other.x, y * other.y)

  def dot(other: Vec2): Double = x * other.x + y * other.y
  def cross(other: Vec2): Double = x * other.y - other.x * y

  def ->(other: Vec2): Vec2 = other - this

  def lengthSquared: Double = x * x + y * y
  def length: Double = math.sqrt(lengthSquared)
  def normed: Vec2 = if (length == 0) Vec2(0, 0) else this / length

  def toAngle: Double = -math.atan2(y, x)

  def perp = Vec2(-y, x)

  def lerp(other: Vec2, t: Double): Vec2 = this * (1 - t) + other * t

  def rotate(angle: Double): Vec2 = Mat33.rotate(angle) * this

  override def toString: String = f"$productPrefix%s($x%.2f,$y%.2f)"
}

object Vec2 {
  def forAngle(t: Double) = Vec2(math.cos(t), -math.sin(t))
  def aroundCircle(numPoints: Int, startAngle: Double = 0): Seq[Vec2] =
    for (i <- 0 until numPoints) yield Vec2.forAngle(i.toDouble / numPoints * 2 * math.Pi + startAngle)
}

case class Vec3(x: Double, y: Double, z: Double) {
  def toVec2: Vec2 = Vec2(x, y)

  def +(other: Vec3): Vec3 = Vec3(x + other.x, y + other.y, z + other.z)
  def -(other: Vec3): Vec3 = Vec3(x - other.x, y - other.y, z - other.z)
  def *(k: Double): Vec3 = Vec3(x * k, y * k, z * k)
  def /(k: Double): Vec3 = Vec3(x / k, y / k, z / k)

  def dot(other: Vec3): Double = x * other.x + y * other.y + z * other.z
  def cross(other: Vec3): Vec3 = Vec3(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x
  )


  def ->(other: Vec3): Vec3 = other - this

  def lengthSquared: Double = x * x + y * y + z * z
  def length: Double = math.sqrt(lengthSquared)
  def normed: Vec3 = if (length == 0) Vec3(0, 0, 0) else this / length
}

case class Vec4(x: Double, y: Double, z: Double, w: Double) {
  def +(other: Vec4): Vec4 = Vec4(x + other.x, y + other.y, z + other.z, w + other.w)
  def -(other: Vec4): Vec4 = Vec4(x - other.x, y - other.y, z - other.z, w - other.w)
  def *(k: Double): Vec4 = Vec4(x * k, y * k, z * k, w * k)
  def /(k: Double): Vec4 = Vec4(x / k, y / k, z * k, w * k)

  def dot(other: Vec4): Double = x * other.x + y * other.y + z * other.z + w * other.w

  def ->(other: Vec4): Vec4 = other - this

  def lengthSquared: Double = x * x + y * y + z * z + w * w
  def length: Double = math.sqrt(lengthSquared)
  def normed: Vec4 = if (length == 0) Vec4(0, 0, 0, 0) else this / length
}


/** 2x2 Matrix
  * ( a  b )
  * ( c  d )
  */
case class Mat22(a: Double, b: Double, c: Double, d: Double) {
  def *(v: Vec2): Vec2 = Vec2(a * v.x + b * v.y, c * v.x + d * v.y)
  def *(m: Mat22): Mat22 = Mat22(
    a * m.a + b * m.c, a * m.b + b * m.d,
    c * m.a + d * m.c, c * m.b + d * m.d
  )

  def determinant: Double = a * d - b * c
}

/** 3x3 Matrix
  * ( a  b  c )
  * ( d  e  f )
  * ( g  h  i )
  */
case class Mat33(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double, g: Double, h: Double, i: Double) {
  def *(v: Vec3): Vec3 = Vec3(
    a * v.x + b * v.y + c * v.z,
    d * v.x + e * v.y + f * v.z,
    g * v.x + h * v.y + i * v.z
  )
  def *(m: Mat33): Mat33 = Mat33(
    a * m.a + b * m.d + c * m.g, a * m.b + b * m.e + c * m.h, a * m.c + b * m.f + c * m.i,
    d * m.a + e * m.d + f * m.g, d * m.b + e * m.e + f * m.h, d * m.c + e * m.f + f * m.i,
    g * m.a + h * m.d + i * m.g, g * m.b + h * m.e + i * m.h, g * m.c + h * m.f + i * m.i
  )
  def *(k: Double): Mat33 = Mat33(
    a * k, b * k, c * k,
    d * k, e * k, f * k,
    g * k, h * k, i * k
  )

  def *(v: Vec2): Vec2 = this * Vec3(v.x, v.y, 1) match { case Vec3(x, y, _) => Vec2(x, y) }

  def inverse = {
    val ai = e * i - f * h
    val bi = -(d * i - f * g)
    val ci = d * h - e * g
    val di = -(b * i - c * h)
    val ei = a * i - c * g
    val fi = -(a * h - b * g)
    val gi = b * f - c * e
    val hi = -(a * f - c * d)
    val ii = a * e - b * d
    val det = a * ai + b * bi + c * ci
    if (det == 0)
      throw new RuntimeException(s"Singular matrix can't be inverted: $this")
    Mat33(
      ai, di, gi,
      bi, ei, hi,
      ci, fi, ii
    ) * (1 / det)
  }

  def determinant = {
    val ai = e * i - f * h
    val bi = -(d * i - f * g)
    val ci = d * h - e * g
    a * ai + b * bi + c * ci
  }

  def toSeq: Seq[Double] = Seq(a, b, c, d, e, f, g, h, i)
}
object Mat33 {
  def identity: Mat33 = Mat33(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  )
  def translate(tx: Double, ty: Double): Mat33 = Mat33(
    1, 0, tx,
    0, 1, ty,
    0, 0, 1
  )
  def translate(v: Vec2): Mat33 = translate(v.x, v.y)
  def rotate(theta: Double): Mat33 = {
    val c = math.cos(theta)
    val s = -math.sin(theta)
    Mat33(
      c, -s, 0,
      s, c, 0,
      0, 0, 1
    )
  }
  def scale(k: Double): Mat33 = {
    Mat33(
      k, 0, 0,
      0, k, 0,
      0, 0, 1
    )
  }
  def scale(x: Double, y: Double): Mat33 = {
    Mat33(
      x, 0, 0,
      0, y, 0,
      0, 0, 1
    )
  }
  def scale(v: Vec2): Mat33 = scale(v.x, v.y)
}

/** 4x4 Matrix
  * ( a  b  c  d )
  * ( e  f  g  h )
  * ( i  j  k  l )
  * ( m  n  o  p )
  */
case class Mat44(
  a: Double, b: Double, c: Double, d: Double,
  e: Double, f: Double, g: Double, h: Double,
  i: Double, j: Double, k: Double, l: Double,
  m: Double, n: Double, o: Double, p: Double
) {
  def *(v: Vec4): Vec4 = Vec4(
    a * v.x + b * v.y + c * v.z + d * v.w,
    e * v.x + f * v.y + g * v.z + h * v.w,
    i * v.x + j * v.y + k * v.z + l * v.w,
    m * v.x + n * v.y + o * v.z + p * v.w
  )
  def *(z: Mat44): Mat44 = Mat44(
    a * z.a + b * z.e + c * z.i + d * z.m, a * z.b + b * z.f + c * z.j + d * z.n, a * z.c + b * z.g + c * z.k + d * z.o, a * z.d + b * z.h + c * z.l + d * z.p,
    e * z.a + f * z.e + g * z.i + h * z.m, e * z.b + f * z.f + g * z.j + h * z.n, e * z.c + f * z.g + g * z.k + h * z.o, e * z.d + f * z.h + g * z.l + h * z.p,
    i * z.a + j * z.e + k * z.i + l * z.m, i * z.b + j * z.f + k * z.j + l * z.n, i * z.c + j * z.g + k * z.k + l * z.o, i * z.d + j * z.h + k * z.l + l * z.p,
    m * z.a + n * z.e + o * z.i + p * z.m, m * z.b + n * z.f + o * z.j + p * z.n, m * z.c + n * z.g + o * z.k + p * z.o, m * z.d + n * z.h + o * z.l + p * z.p
  )
  def *(z: Double): Mat44 = Mat44(
    a * z, b * z, c * z, d * z,
    e * z, f * z, g * z, h * z,
    i * z, j * z, k * z, l * z,
    m * z, n * z, o * z, p * z
  )

  def *(v: Vec3): Vec3 = this * Vec4(v.x, v.y, v.z, 1) match { case Vec4(x, y, z, w) => Vec3(x / w, y / w, z / w) }

  def toSeq: Seq[Double] = Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
}
object Mat44 {
  def identity: Mat44 = Mat44(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  )
  def rotate(xp: Double, yp: Double, zp: Double, a: Double): Mat44 = {
    val c = math.cos(a)
    val s = -math.sin(a)
    val Vec3(x, y, z) = Vec3(xp, yp, zp).normed
    Mat44(
      x*x*(1-c)+c, x*y*(1-c)-z*s, x*z*(1-c)+y*s, 0,
      y*x*(1-c)+z*s, y*y*(1-c)+c, y*z*(1-c)-x*s, 0,
      x*z*(1-c)-y*s, y*z*(1-c)+x*s, z*z*(1-c)+c, 0,
      0, 0, 0, 1
    )
  }
  def translate(tx: Double, ty: Double, tz: Double): Mat44 = Mat44(
    1, 0, 0, tx,
    0, 1, 0, ty,
    0, 0, 1, tz,
    0, 0, 0, 1
  )
  def translate(v: Vec3): Mat44 = translate(v.x, v.y, v.z)
  def scale(k: Double): Mat44 = {
    Mat44(
      k, 0, 0, 0,
      0, k, 0, 0,
      0, 0, k, 0,
      0, 0, 0, 1
    )
  }
  def scale(x: Double, y: Double, z: Double): Mat44 = {
    Mat44(
      x, 0, 0, 0,
      0, y, 0, 0,
      0, 0, z, 0,
      0, 0, 0, 1
    )
  }
  def scale(v: Vec3): Mat44 = scale(v.x, v.y, v.z)

  // https://github.com/jpbetz/subspace/blob/master/subspace/src/main/scala/com/github/jpbetz/subspace/Matrix4x4.scala
  def perspective(fovRad: Double, aspect: Double, near: Double, far: Double): Mat44 = {
    val fov = 1 / math.tan(fovRad / 2f).toFloat
    Mat44(
      fov / aspect, 0, 0, 0,
      0, fov, 0, 0,
      0, 0, (far + near) / (near - far), -1,
      0, 0, (2 * far * near) / (near - far), 0)
  }
}

case class Plane3(dir: Vec3, d: Double) {
  def basis: (Vec3, Vec3) = {
    val b1 = dir match {
      case Vec3(0, _, _) => Vec3(1, 0, 0)  // parallel to x axis
      case Vec3(_, 0, _) => Vec3(0, 1, 0)  // parallel to y axis
      case Vec3(_, _, 0) => Vec3(0, 0, 1)  // parallel to z axis
      case _ => Vec3(-dir.y, dir.x, 0).normed
    }
    val b2 = dir cross b1
    (b1, b2)
  }
}
object Plane3 {
  def fromPointAndDir(point: Vec3, dir: Vec3): Plane3 =
    Plane3(dir.normed, -(point dot dir.normed))

  def fromPoints(p: Vec3, q: Vec3, r: Vec3): Plane3 = {
    val dir = ((p - r) cross (q - r)).normed
    Plane3(dir, -(dir dot r))
  }
}

case class Tri3(a: Vec3, b: Vec3, c: Vec3) {
  /** Radius of circle passing through a,b,c */
  def radius: Double = {
    val lengths = (a -> b).length * (b -> c).length * (c -> a).length
    val area = ((a -> b) cross (a -> c)).length / 2
    lengths / 4 * area
  }

  def plane: Plane3 = Plane3.fromPoints(a, b, c)

  def circle: Circle3 = Circle3((a + b + c) / 3, radius, (a -> b) cross (a -> c))
}

sealed trait Shape2 {
}


/** A circle of radius `r` centered at `c`. */
case class Circle2(c: Vec2, r: Double) extends Shape2 {
  def toSVG: String =
    s"M ${c.x} ${c.y} m ${-r} 0 a $r,$r 0 1,1 ${r*2},0 a $r,$r 0 1,1 ${-r*2},0"

  def boundingBox = AABB(c - Vec2(r, r), c + Vec2(r, r))

  def contains(p: Vec2): Boolean = (c -> p).lengthSquared <= r*r

  def toPolygon(numPoints: Int, startAngle: Double = 0): Polygon =
    Polygon(Vec2.aroundCircle(numPoints, startAngle).map(_ * r)).translate(c)

  def area: Double = math.Pi * r * r
}

case class Circle3 private (s: Sphere3, p: Plane3) {
  def toPolygon(numPoints: Int): Polygon3 = {
    val (a, b) = p.basis
    Polygon3(
      for (i <- 0 until numPoints)
        yield (
          a * math.cos(i.toDouble / numPoints * math.Pi * 2) +
          b * math.sin(i.toDouble / numPoints * math.Pi * 2)
        ) * s.r + s.c
    )
  }
}
object Circle3 {
  def apply(center: Vec3, radius: Double, direction: Vec3): Circle3 =
    Circle3(Sphere3(center, radius), Plane3.fromPointAndDir(center, direction))
}

case class Sphere3(c: Vec3, r: Double)

case class Arc2(c: Vec2, rx: Double, ry: Double, rotation: Double, startAngle: Double, sweptAngle: Double) extends Shape2 {
  assert(rx == ry)
  assert(rotation == 0)
  // https://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
  def toSVGArc: SVGArc = {
    val rotate = Mat33.rotate(rotation)
    val r = Vec2(rx, ry)
    val v0 = rotate * (r o Vec2.forAngle(startAngle)) + c
    val v1 = rotate * (r o Vec2.forAngle(startAngle + sweptAngle)) + c
    SVGArc(v0, v1, rx, ry, rotation, sweptAngle.abs > math.Pi, sweptAngle > 0)
  }

  def translate(p: Vec2): Arc2 = copy(c = c + p)
  def rotateAboutOrigin(angle: Double): Arc2 = {
    if (c == Vec2(0, 0))
      copy(startAngle = startAngle + angle)
    else
      toSVGArc.rotateAboutOrigin(angle).toArc2
  }
  def scale(k: Double) = copy(c = c * k, rx = rx * k, ry = ry * k)

  /** t in [0, 1] */
  def sample(t: Double): Vec2 = Mat33.rotate(rotation) * (Vec2(rx, ry) o Vec2.forAngle(startAngle + sweptAngle * t)) + c
  def toPoints(numPoints: Int): Seq[Vec2] = (0 until numPoints) map { i => sample(i/(numPoints - 1).toDouble) }

  def toSVG: String = toSVGArc.toSVG
  /*def toSVG: String = {
    val r = Vec2(rx, ry)
    val start = c + (r o Vec2.forAngle(startAngle))
    val end = c + (r o Vec2.forAngle(startAngle + sweptAngle))
    s"M${start.x} ${start.y} A $rx $ry 0 0 1 ${end.x} ${end.y}"
  }*/
}

case class SVGArc(start: Vec2, end: Vec2, rx: Double, ry: Double, xRot: Double, largeArc: Boolean, sweep: Boolean) {
  def translate(p: Vec2): SVGArc = copy(start = start + p, end = end + p)
  def rotateAbout(p: Vec2, angle: Double): SVGArc = translate(-p).rotateAboutOrigin(angle).translate(p)
  def rotateAboutOrigin(angle: Double): SVGArc = {
    val mat = Mat33.rotate(angle)
    copy(start = mat * start, end = mat * end)
  }
  def scale(k: Double) = copy(start = start * k, end = end * k, rx = rx * k, ry = ry * k)

  // https://svgwg.org/svg2-draft/implnote.html#ArcImplementationNotes
  def toArc2: Arc2 = {
    val v1p = Mat33.rotate(xRot) * ((start - end) / 2)
    val sgn = if (largeArc != sweep) 1 else -1
    //val radiiCheck = math.pow(v1p.x, 2)/math.pow(this.rx, 2) + math.pow(v1p.y, 2)/math.pow(this.ry, 2)
    //val rx = if (radiiCheck > 1) math.sqrt(radiiCheck) * this.rx else this.rx
    //val ry = if (radiiCheck > 1) math.sqrt(radiiCheck) * this.ry else this.ry
    val radicand = (rx*rx * ry*ry - rx*rx * v1p.y*v1p.y - ry*ry * v1p.x*v1p.x) / (rx*rx * v1p.y*v1p.y + ry*ry * v1p.x*v1p.x)
    val cp = Vec2(
      rx*v1p.y / ry,
      -ry*v1p.x / rx
    ) * (math.sqrt(radicand max 0) * sgn)
    val c = Mat33.rotate(-xRot) * cp + (start + end) / 2
    def angleBetween(u: Vec2, v: Vec2) = { v.toAngle - u.toAngle }
    val recipRadius = Vec2(1 / rx, 1 / ry)
    val v = (v1p - cp) o recipRadius
    val startAngle = angleBetween(Vec2(1, 0), v)
    val dTheta = angleBetween(v, (-v1p - cp) o recipRadius)
    val sweptAngle = (
      if (!sweep && dTheta > 0) dTheta - 2 * math.Pi
      else if (sweep && dTheta < 0) dTheta + 2 * math.Pi
      else dTheta
    ) % (2 * math.Pi)
    Arc2(c, rx, ry, xRot, startAngle, sweptAngle)
  }

  def toSVG: String = s"M${start.x} ${start.y} A$rx $ry $xRot ${if (largeArc) 1 else 0} ${if (sweep) 1 else 0} ${end.x} ${end.y}"
}

/** A segment beginning at `a` and ending at `b`. */
case class Segment2(a: Vec2, b: Vec2) extends Shape2 {
  def left: Vec2 = if (a.x < b.x) a else b
  def right: Vec2 = if (a.x < b.x) b else a

  lazy val slope: Double = if (b.x == a.x) Double.PositiveInfinity else (b.y - a.y) / (b.x - a.x)
  lazy val yIntercept: Double = if (b.x == a.x) Double.PositiveInfinity else a.y - slope * a.x
  def yAtX(x: Double): Double = slope * x + yIntercept

  def reverse: Segment2 = Segment2(b, a)

  def translate(v: Vec2): Segment2 = Segment2(a + v, b + v)

  /** The point on this segment closest to `p`. */
  def closestPointTo(p: Vec2): Vec2 = {
    val l2 = (a - b).lengthSquared
    if (l2 == 0) return a
    val t = math.max(0, math.min(1, ((p - a) dot (b - a)) / l2))
    a + (b - a) * t
  }

  def length: Double = (a -> b).length

  def toRectangle(width: Double): Polygon = {
    val sideways = (a -> b).perp.normed * (width / 2)
    Polygon(Seq(
      a + sideways,
      b + sideways,
      b - sideways,
      a - sideways
    ))
  }

  def sample(t: Double) = a.lerp(b, t)
  def toPoints(numPoints: Int): Seq[Vec2] = (0 until numPoints) map { i => sample(i/(numPoints - 1).toDouble) }

  def toSVG: String = s"M${a.x},${a.y} L${b.x},${b.y}"
}

case class Segment3(a: Vec3, b: Vec3) {
  def reverse: Segment3 = Segment3(b, a)

  def translate(v: Vec3): Segment3 = Segment3(a + v, b + v)

  def length: Double = (a -> b).length
}


/** Closed polygon. */
case class Polygon(points: Seq[Vec2]) extends Shape2 {
  /** Sequence of points representing the vertices of this polygon, with the final point equal to the first. */
  def toPolyLine: Seq[Vec2] = points :+ points.head

  /** Translate all the points in the polygon by `offset`. */
  def translate(offset: Vec2): Polygon = Polygon(points map (_ + offset))
  /** Rotate all the points in the polygon about Vec2(0, 0). */
  def rotateAroundOrigin(angle: Double): Polygon = Polygon(points map (_.rotate(angle)))
  /** Scale all the points in the polygon by `k`. */
  def scale(k: Double): Polygon = Polygon(points map (_ * k))

  def transform(mat: Mat33): Polygon = Polygon(points map (mat * _))

  // TODO: this might actually be isCW?
  def isCCW: Boolean = (points ++ points.takeRight(2)).sliding(3).forall {
    case Seq(a, b, c) =>
      ((a -> b) cross (b -> c)) <= 0
    case _ => true
  }

  def toCCWPolyLine = if (isCCW) toPolyLine else toPolyLine.reverse

  /** Area of the polygon, assuming its segments are non-intersecting. */
  def area: Double = (segments.foldLeft(0.0) { (a, s) => a + (s.a cross s.b) } / 2).abs

  /** A sequence of segments representing the edges of this polygon. */
  def segments = toPolyLine.sliding(2) map { case Seq(a, b) => Segment2(a, b) }

  /** The average of the vertices of this polygon. */
  def centroid: Vec2 = points.reduce(_ + _) / points.size

  lazy val aabb: AABB = {
    var lowerX = points.head.x
    var lowerY = points.head.y
    var upperX = points.head.x
    var upperY = points.head.y
    for (p <- points.tail) {
      if (p.x < lowerX) lowerX = p.x
      if (p.x > upperX) upperX = p.x
      if (p.y < lowerY) lowerY = p.y
      if (p.y > upperY) upperY = p.y
    }
    AABB(Vec2(lowerX, lowerY), Vec2(upperX, upperY))
  }

  def toSVG: String = {
    val h = points.head
    val t = points.tail
    s"M${h.x},${h.y} ${t.map(p => s"L${p.x},${p.y}").mkString(" ")} Z"
  }
}

object Polygon {
  /** A square of side length `side` centered at the origin. */
  def square(side: Double): Polygon =
    rectangle(side, side)

  def rectangle(width: Double, height: Double): Polygon = {
    Polygon(Seq(
      Vec2(-width/2, -height/2),
      Vec2(width/2, -height/2),
      Vec2(width/2, height/2),
      Vec2(-width/2, height/2)
    ))
  }
}

case class Polygon3(points: Seq[Vec3]) {
  /** Sequence of points representing the vertices of this polygon, with the final point equal to the first. */
  def toPolyLine: Seq[Vec3] = points :+ points.head

  /** Translate all the points in the polygon by `offset`. */
  def translate(offset: Vec3): Polygon3 = Polygon3(points map (_ + offset))
  /** Scale all the points in the polygon by `k`. */
  def scale(k: Double): Polygon3 = Polygon3(points map (_ * k))

  def transform(mat: Mat44): Polygon3 = Polygon3(points map (mat * _))

  /** A sequence of segments representing the edges of this polygon. */
  def segments = toPolyLine.sliding(2) map { case Seq(a, b) => Segment3(a, b) }

  /** The average of the vertices of this polygon. */
  def centroid: Vec3 = points.reduce(_ + _) / points.size
}

object AABB {
  def apply(lowerX: Double, lowerY: Double, upperX: Double, upperY: Double): AABB =
    AABB(Vec2(lowerX, lowerY), Vec2(upperX, upperY))
}

/** Axis-aligned bounding box.
  *
  * `lower` must be <= `upper` in both dimensions.
  */
case class AABB(lower: Vec2, upper: Vec2) extends Shape2 {
  require(lower.x <= upper.x && lower.y <= upper.y, s"Invalid AABB: $lower must be <= $upper")

  def width: Double = upper.x - lower.x
  def height: Double = upper.y - lower.y
  def maxDimension: Double = width max height
  def minDimension: Double = width min height
  def center: Vec2 = lower + (upper - lower) * 0.5

  def toPolygon: Polygon = Polygon(Seq(lower, lower.copy(x = upper.x), upper, lower.copy(y = upper.y)))
  def segments: Seq[Segment2] = toPolygon.segments.toSeq

  // TODO: better names for the below
  private def ll = lower
  private def ul = Vec2(upper.x, lower.y)
  private def uu = upper
  private def lu = Vec2(lower.x, upper.y)

  def corners: Seq[Vec2] = Seq(ll, ul, uu, lu)

  private def topEdge = Segment2(ll, ul)
  private def rightEdge = Segment2(ul, uu)
  private def bottomEdge = Segment2(uu, lu)
  private def leftEdge = Segment2(lu, ll)

  /** True if `point` is contained within the AABB.
    *
    * Points exactly on the edge of the box are considered to be within the box.
    */
  def contains(point: Vec2): Boolean =
    point.x >= lower.x && point.x <= upper.x && point.y >= lower.y && point.y <= upper.y

  def contains(circle: Circle2): Boolean = {
    val Circle2(c, r) = circle
    c.x - r >= lower.x && c.x + r <= upper.x && c.y - r >= lower.y && c.y + r <= upper.y
  }

  def contains(polygon: Polygon): Boolean = polygon.points.forall(this.contains)

  def contains(seg: Segment2): Boolean = contains(seg.a) && contains(seg.b)

  def liangBarsky(seg: Segment2): Option[Vec2] = {
    // Liang-Barsky
    // https://gist.github.com/ChickenProp/3194723
    val a = seg.a
    val delta = a -> seg.b
    val p = Seq(-delta.x, delta.x, -delta.y, delta.y)
    val q = Seq(a.x - lower.x, upper.x - a.x, a.y - lower.y, upper.y - a.y)
    var u1 = Double.NegativeInfinity
    var u2 = Double.PositiveInfinity

    for (i <- 0 until 4) {
      if (p(i) == 0) {
        if (q(i) < 0)
          return None
      } else {
        val t = q(i) / p(i)
        if (p(i) < 0 && u1 < t)
          u1 = t
        else if (p(i) > 0 && u2 > t)
          u2 = t
      }
    }

    if (u1 > u2 || u1 > 1 || u1 < 0)
      return None

    Some(a + delta * u1)
  }

  /** Returns the largest subsegment that's completely contained within the AABB, if one exists. */
  def truncate(segment: Segment2): Option[Segment2] = {
    val containsA = contains(segment.a)
    val containsB = contains(segment.b)
    if (containsA && containsB) return Some(segment)
    if (containsA && !containsB) return Some(Segment2(segment.a, liangBarsky(segment.reverse).get))
    if (!containsA && containsB) return Some(Segment2(liangBarsky(segment).get, segment.b))
    val forwards = liangBarsky(segment)
    val backwards = liangBarsky(segment.reverse)
    if (forwards.isEmpty || backwards.isEmpty)
      None
    else
      Some(Segment2(forwards.get, backwards.get))
  }

  def truncate(polygon: Polygon): Seq[Segment2] = polygon.segments.flatMap(truncate(_)).toSeq

  /** Returns the closest point to `point` that's inside the AABB. */
  def clip(point: Vec2): Vec2 = Vec2(
    math.max(lower.x, math.min(upper.x, point.x)),
    math.max(lower.y, math.min(upper.y, point.y))
  )

  def expand(k: Double): AABB = AABB(lower - Vec2(k, k), upper + Vec2(k, k))
  def shrink(k: Double): AABB = AABB(lower + Vec2(k, k), upper - Vec2(k, k))

  def translate(v: Vec2): AABB = AABB(lower + v, upper + v)

  def subdivided(x: Int, y: Int): Seq[Vec2] =
    for (i <- 0 until y; j <- 0 until x) yield lower + Vec2(width / x * j, height / y * i)

}
