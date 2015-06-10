scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

name := "sonofjson"

version := "0.1"

organization := "nl.typeset"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq(
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
      )
    case _ => Seq.empty
  }
}

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.specs2" %% "specs2" % "2.4.2" % "test"
