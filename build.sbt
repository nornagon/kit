lazy val kit = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "net.nornagon",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Kit",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
      "com.lihaoyi" %% "fastparse" % "1.0.0"
    ),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1")
  )
