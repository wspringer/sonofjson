scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.11.4", "2.10.4")

name := "sonofjson"

version := "0.1-SNAPSHOT"

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
