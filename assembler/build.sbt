addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.15")

lazy val root = (project in file(".")).
  settings(
  name := "assembler",
  version := "1.0",
  scalaVersion := "2.11.8",
  libraryDependencies += "org.specs2" %% "specs2-core" % "2.4.16" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"
)
