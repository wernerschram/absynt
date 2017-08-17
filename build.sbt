scalaVersion in ThisBuild := "2.12.0"

lazy val root = project in file(".") aggregate(assembler, assemblerX86, assemblerARM, ARMBootRpiExample, X86BootFlagExample)

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

lazy val assembler = (project in file("assembler"))
.settings(
  name := "assembler",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)


lazy val assemblerX86 = (project in file("assembler.x86"))
.settings(
  name := "assembler.x86",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
).dependsOn(assembler)

lazy val assemblerARM = (project in file("assembler.arm")) 
.settings(
  name := "assembler.arm",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
).dependsOn(assembler)

lazy val ARMBootRpiExample = (project in file("examples/arm/bootRpi")) 
.settings(
  name := "assembler.examples.arm.bootRpi",
  version := "1.0"
).dependsOn(assembler, assemblerARM)

lazy val X86BootFlagExample = (project in file("examples/x86/bootFlag"))
.settings(
  name := "assembler.examples.x86.bootFlag",
  version := "1.0"
).dependsOn(assembler, assemblerX86)

