package kit

import java.io.Reader

import scala.xml.{Node, XML}

object SVG {
  /*
  def parseFigma(in: Reader): Option[Elem] = {
    val svg = XML.load(in)
    val width = (svg \@ "width").toDouble
    val height = (svg \@ "height").toDouble
    val Array(l, t, r, b) = (svg \@ "viewBox").split("\\s+")
    val defsById: Map[String, Node] = (for (e <- svg \ "defs" \ "_") yield (e \@ "id") -> e)(collection.breakOut)
    parseNode(svg)(defsById)
  }

  private val idRegex = raw"""#(.+)""".r

  private def parseNode(node: Node)(implicit defs: Map[String, Node]): Option[Elem] = {
    node match {
      case g @ <g>{children @ _*}</g> =>
        val clipPath = g \@ "clip-path"
        Some(Group(
          transform = parseTransform(g \@ "transform"),
          clipPath = None,
          children = children.flatMap(parseNode(_)),
          style = Style(fill = g \@ "fill", stroke = g \@ "stroke")
        ))
      case path @ <path/> =>
        val d = MicroSyntax.SVGPathData.parse(path \@ "d").fold(
          (_, _, extra) => throw new RuntimeException(s"Couldn't parse path: $extra"),
          (commandGroups, _) => commandGroups)
        Some(Path(d = d))
      case use @ <use/> =>
        val referencedId = use \@ "{http://www.w3.org/1999/xlink}href" match { case idRegex(id) => id; case o => throw new RuntimeException(s"$o") }
        Some(Group(
          transform = parseTransform(use \@ "transform"),
          clipPath = None,
          children = parseNode(defs(referencedId)).toSeq,
          style = Style(fill = use \@ "fill", stroke = use \@ "stroke")
        ))
      case svg @ <svg>{children @ _*}</svg> =>
        Some(Group(
          transform = Mat33.identity,
          clipPath = None,
          children = children.flatMap(parseNode(_)),
          style = Style(fill = "", stroke = "")
        ))
      case _ => None
    }
  }

  private def parseTransform(transform: String): Mat33 = {
    MicroSyntax.SVGTransform.parse(transform).fold(
      (_, _, extra) => throw new RuntimeException(s"Couldn't parse transform: $extra"),
      (mat, _) => mat
    )
  }

  case class Doc()

  sealed trait Elem
  case class Group(
    transform: Mat33,
    clipPath: Option[Path],
    children: Seq[Elem],
    style: Style
  ) extends Elem
  case class Path(
    d: Seq[MicroSyntax.SVGPathData.CommandGroup]
  ) extends Elem
  case class Style(
    fill: String,
    stroke: String
  )
  */
}

//noinspection TypeAnnotation
object MicroSyntax {
  import fastparse.all._

  object SVGPathData {
    case class MoveTo(coords: Seq[Vec2], relative: Boolean)
    sealed trait DrawToCommand
    case class ClosePath() extends DrawToCommand
    case class LineTo(coords: Seq[Vec2], relative: Boolean) extends DrawToCommand
    case class HorizontalLineTo(xs: Seq[Double], relative: Boolean) extends DrawToCommand
    case class VerticalLineTo(ys: Seq[Double], relative: Boolean) extends DrawToCommand
    case class CurveTo(coords: Seq[(Vec2, Vec2, Vec2)], relative: Boolean) extends DrawToCommand
    case class SmoothCurveTo(coords: Seq[(Vec2, Vec2)], relative: Boolean) extends DrawToCommand
    case class QuadraticBezierCurveTo(coords: Seq[(Vec2, Vec2)], relative: Boolean) extends DrawToCommand
    case class SmoothQuadraticBezierCurveTo(coords: Seq[Vec2], relative: Boolean) extends DrawToCommand
    case class EllipticalArc(coords: Seq[(Vec2, Double, Boolean, Boolean, Vec2)], relative: Boolean) extends DrawToCommand

    case class CommandGroup(moveTo: MoveTo, drawTos: Seq[DrawToCommand])

    def parse(pathData: String) = (`svg-path` ~ End).parse(pathData)

    // https://www.w3.org/TR/SVG/paths.html#PathData
    lazy val `svg-path`: Parser[Seq[CommandGroup]] = P(wsp.rep ~ `moveto-drawto-command-groups`.?.map(_.getOrElse(Seq.empty)) ~ wsp.rep)
    lazy val `moveto-drawto-command-groups` = P(`moveto-drawto-command-group`.rep(min = 1, sep = wsp.rep))
    lazy val `moveto-drawto-command-group` = P(moveto ~ wsp.rep ~ `drawto-commands`.?.map(_.getOrElse(Seq.empty))).map {
      case (moveTo, drawTos) => CommandGroup(moveTo, drawTos)
    }
    lazy val `drawto-commands` = P(`drawto-command`.rep(min = 1, sep = wsp.rep))
    lazy val `drawto-command`: Parser[DrawToCommand] = P(
      closepath
        | lineto
        | `horizontal-lineto`
        | `vertical-lineto`
        | curveto
        | `smooth-curveto`
        | `quadratic-bezier-curveto`
        | `smooth-quadratic-bezier-curveto`
        | `elliptical-arc`
    )
    lazy val `moveto` = P(CharIn("Mm").! ~/ wsp.rep ~ `moveto-argument-sequence`).map {
      case (c, coords) => MoveTo(coords, c.head.isLower) }
    lazy val `moveto-argument-sequence` = P(`coordinate-pair`.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `closepath` = P(CharIn("Zz").!).map(_ => ClosePath())
    lazy val `lineto` = P(CharIn("Ll").! ~/ wsp.rep ~ `lineto-argument-sequence`).map {
      case (c, coords) => LineTo(coords, c.head.isLower) }
    lazy val `lineto-argument-sequence` = P(`coordinate-pair`.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `horizontal-lineto` = P(CharIn("Hh").! ~/ wsp.rep ~ `horizontal-lineto-argument-sequence`).map {
      case (c, coords) => HorizontalLineTo(coords, c.head.isLower)
    }
    lazy val `horizontal-lineto-argument-sequence` = P(coordinate.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `vertical-lineto` = P(CharIn("Vv").! ~/ wsp.rep ~ `vertical-lineto-argument-sequence`).map {
      case (c, coords) => VerticalLineTo(coords, c.head.isLower)
    }
    lazy val `vertical-lineto-argument-sequence` = P(coordinate.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `curveto` = P(CharIn("Cc").! ~/ wsp.rep ~ `curveto-argument-sequence`).map {
      case (c, coords) => CurveTo(coords, c.head.isLower)
    }
    lazy val `curveto-argument-sequence` = P(`curveto-argument`.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `curveto-argument` = P(`coordinate-pair` ~ `comma-wsp`.? ~ `coordinate-pair` ~ `comma-wsp`.? ~ `coordinate-pair`)
    lazy val `smooth-curveto` = P(CharIn("Ss").! ~/ wsp.rep ~ `smooth-curveto-argument-sequence`).map {
      case (c, coords) => SmoothCurveTo(coords, c.head.isLower)
    }
    lazy val `smooth-curveto-argument-sequence` = P(`smooth-curveto-argument`.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `smooth-curveto-argument` = P(`coordinate-pair` ~ `comma-wsp`.? ~ `coordinate-pair`)
    lazy val `quadratic-bezier-curveto` = P(CharIn("Qq").! ~/ wsp.rep ~ `quadratic-bezier-curveto-argument-sequence`).map {
      case (c, coords) => QuadraticBezierCurveTo(coords, c.head.isLower)
    }
    lazy val `quadratic-bezier-curveto-argument-sequence` = P(`quadratic-bezier-curveto-argument`.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `quadratic-bezier-curveto-argument` = P(`coordinate-pair` ~ `comma-wsp`.? ~ `coordinate-pair`)
    lazy val `smooth-quadratic-bezier-curveto` = P(CharIn("Tt").! ~/ wsp.rep ~ `smooth-quadratic-bezier-curveto-argument-sequence`).map {
      case (c, coords) => SmoothQuadraticBezierCurveTo(coords, c.head.isLower)
    }
    lazy val `smooth-quadratic-bezier-curveto-argument-sequence` = P(`coordinate-pair`.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `elliptical-arc` = P(CharIn("Aa").! ~/ wsp.rep ~ `elliptical-arc-argument-sequence`).map {
      case (c, coords) => EllipticalArc(coords, c.head.isLower)
    }
    lazy val `elliptical-arc-argument-sequence` = P(`elliptical-arc-argument`.rep(min = 1, sep = `comma-wsp`.?))
    lazy val `elliptical-arc-argument` = P(
      (`nonnegative-number` ~ `comma-wsp`.? ~ `nonnegative-number`).map { case (x, y) => Vec2(x, y) } ~ `comma-wsp`.?
        ~ number ~ `comma-wsp` ~ flag ~ `comma-wsp`.? ~ flag ~ `comma-wsp`.? ~ `coordinate-pair`
    )
    lazy val `coordinate-pair` = P(coordinate ~ `comma-wsp`.? ~ coordinate).map { case (x, y) => Vec2(x, y) }
    lazy val coordinate = number
    lazy val `nonnegative-number` = P(`integer-constant` | `floating-point-constant`).!.map(_.toDouble)
  }

  object SVGTransform {
    def parse(transformList: String) = (`transform-list` ~ End).parse(transformList)
    // https://www.w3.org/TR/SVG/coords.html#TransformAttribute
    lazy val `transform-list` = P(wsp.rep ~ transforms.?.map(_.getOrElse(Mat33.identity)) ~ wsp.rep)
    lazy val transforms = P(transform.rep(min = 1, sep = `comma-wsp`.rep(min = 1))).map { ms =>
      ms.foldLeft(Mat33.identity) { (m, o) => m * o }
    }
    lazy val transform = P(matrix | translate | scale | rotate | skewX | skewY)
    lazy val matrix = P("matrix" ~/ wsp.rep ~ "(" ~/ wsp.rep
      ~ number ~ `comma-wsp`
      ~ number ~ `comma-wsp`
      ~ number ~ `comma-wsp`
      ~ number ~ `comma-wsp`
      ~ number ~ `comma-wsp`
      ~ number ~ wsp.rep ~ ")").map {
      case (a, b, c, d, e, f) => Mat33(a, c, e, b, d, f, 0, 0, 1)
    }
    lazy val translate = P("translate" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ (`comma-wsp` ~ number).? ~ wsp.rep ~ ")").map {
      case (tx, ty) => Mat33.translate(tx, ty.getOrElse(0))
    }
    lazy val scale = P("scale" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ (`comma-wsp` ~ number).? ~ wsp.rep ~ ")").map {
      case (sx, sy) => Mat33.scale(sx, sy.getOrElse(sx))
    }
    lazy val rotate = P("rotate" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ (`comma-wsp` ~ number ~ `comma-wsp` ~ number).? ~ wsp.rep ~ ")").map {
      case (angle, Some((cx, cy))) => Mat33.translate(-cx, -cy) * Mat33.rotate(-angle * math.Pi / 180) * Mat33.translate(cx, cy) // TODO: not sure if this should be flipped?
      case (angle, None) => Mat33.rotate(-angle * math.Pi / 180)
    }
    lazy val skewX = P("skewX" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ wsp.rep ~ ")").map { angle => Mat33.skewX(angle) }
    lazy val skewY = P("skewY" ~/ wsp.rep ~ "(" ~/ wsp.rep ~ number ~ wsp.rep ~ ")").map { angle => Mat33.skewX(angle) }
  }

  lazy val number = P((sign.? ~ `floating-point-constant`) | (sign.? ~ `integer-constant`)).!.map(_.toDouble)
  lazy val flag = P(CharIn("01")).!.map(_ == "1")
  lazy val `comma-wsp` = P((wsp.rep(1) ~ comma.? ~ wsp.rep) | (comma ~ wsp.rep))
  lazy val comma = P(",")
  lazy val `integer-constant` = P(`digit-sequence`)
  lazy val `floating-point-constant` = P((`fractional-constant` ~ exponent.?) | (`digit-sequence` ~ exponent))
  lazy val `fractional-constant` = P((`digit-sequence`.? ~ "." ~ `digit-sequence`) | (`digit-sequence` ~ "."))
  lazy val exponent = P(CharIn("eE") ~ sign.? ~ `digit-sequence`)
  lazy val sign = P(CharIn("+-"))
  lazy val `digit-sequence` = P(digit.rep(1))
  lazy val digit = P(CharIn("0123456789"))
  lazy val wsp = P(CharIn("\u0020\u0009\u000D\u000A"))
}
