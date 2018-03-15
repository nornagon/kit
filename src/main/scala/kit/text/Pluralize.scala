package kit.text

import scala.util.matching.Regex

object Pluralize {
  val prepositions = Seq(
    "in", "of", "to", "at", "with", "from", "into", "for", "on", "by"
  )
  // http://users.monash.edu/~damian/papers/HTML/Plurals.html
  val rules: Seq[(Regex, Regex.Match => String)] = Seq(
    ".*(fish|ois|sheep|deer|pox|[A-Z].*ese|itis)$".r -> (_.group(0)),
    "man$".r -> (_ => "men"),
    "([ml])ouse$".r -> (_.group(1) + "ice"),
    "tooth$".r -> (_ => "teeth"),
    "goose$".r -> (_ => "geese"),
    "foot$".r -> (_ => "feet"),
    "zoon$".r -> (_ => "zoa"),
    "([csx])is$".r -> (_.group(1) + "es"),
    // TODO: tables for codices, vertebrae, etc
    "(ch|sh|ss)$".r -> (_.group(1) + "es"),
    "(zz|x)$".r -> (_.group(0) + "es"), // NOTE: not in damian's algorithm, but present in Lingua::EN::Inflect
    "([aeo]l|[^d]ea|ar)f$".r -> (_.group(1) + "ves"),
    "([nlw]i)fe$".r -> (_.group(1) + "ves"),
    "[aoeui]y$".r -> (_.group(0) + "s"),
    "[A-Z].*y$".r -> (_.group(0) + "s"),
    "y$".r -> (_ => "ies"),
    // TODO: category(-o,-os)
    "[aoeui]o$".r -> (_.group(0) + "s"),
    "o$".r -> (_ => "oes"),
    s"^(\\w+)(\\s+(?:${prepositions.mkString("|")})\\s+(?:.*))$$".r -> (m => pluralize(m.group(1)) + m.group(2)),
    "$".r -> (_ => "s"),
  )

  def pluralize(singular: String): String = {
    for ((rule, replacement) <- rules) {
      rule.findFirstMatchIn(singular) foreach { m =>
        val sb = new StringBuilder
        sb.append(singular.substring(0, m.start))
        sb.append(replacement(m))
        return sb.toString()
      }
    }
    singular
  }
}
