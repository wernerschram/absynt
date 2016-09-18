addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.15")

//libraryDependencies ++= "assembler" % "assembler" % "1.0.0"

lazy val assembler = RootProject(file("../assembler"))

lazy val root = (project in file(".")).
  settings(
  name := "assembler.arm",
  version := "1.0",
  scalaVersion := "2.11.8"
).dependsOn(assembler)

