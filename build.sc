import mill._
import mill.scalalib._

object kit extends SbtModule {
  def scalaVersion = "2.12.4"

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-xml:1.0.6",
    ivy"com.lihaoyi::fastparse:1.0.0"
  )
}
