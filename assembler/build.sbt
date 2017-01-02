addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

lazy val root = (project in file(".")).
  settings(
  name := "assembler",
  version := "1.0",
  scalaVersion := "2.12.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
