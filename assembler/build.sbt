addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.15")

lazy val root = (project in file(".")).
  settings(
  name := "assembler",
  version := "1.0",
  scalaVersion := "2.11.8"
)
