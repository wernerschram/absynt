addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

lazy val assembler = RootProject(file("../assembler"))

lazy val root = (project in file(".")).
  settings(
  name := "assembler.x86",
  version := "1.0",
  scalaVersion := "2.11.8",
  libraryDependencies += "org.specs2" %% "specs2-core" % "2.4.16" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
).dependsOn(assembler)

