scalaVersion in ThisBuild := "2.12.8"

lazy val root = project in file(".") aggregate(
  assembler,
  assemblerX86,
  assemblerARM,
  ARMBootRpiExample,
  X86BootFlagExample,
  X86HelloWorld32Example,
  X86HelloWorld64Example
)

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

lazy val assembler = (project in file("assembler"))
.settings(
  name := "assembler",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
)


lazy val assemblerX86 = (project in file("assembler.x86"))
.settings(
  name := "assembler.x86",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
).dependsOn(assembler)

lazy val assemblerARM = (project in file("assembler.arm")) 
.settings(
  name := "assembler.arm",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
).dependsOn(assembler)

lazy val ARMBootRpiExample = (project in file("examples/arm/bootRpi")) 
.settings(
  name := "assembler.examples.arm.bootRpi",
  version := "1.0",
  coverageEnabled := false
).dependsOn(assembler, assemblerARM)

lazy val X86BootFlagExample = (project in file("examples/x86/bootFlag"))
.settings(
  name := "assembler.examples.x86.bootFlag",
  version := "1.0",
  coverageEnabled := false
).dependsOn(assembler, assemblerX86)

lazy val X86HelloWorld32Example = (project in file("examples/x86/helloWorld32bit"))
.settings(
  name := "assembler.examples.x86.helloWorld32bit",
  version := "1.0",
  coverageEnabled := false
).dependsOn(assembler, assemblerX86)

lazy val X86HelloWorld64Example = (project in file("examples/x86/helloWorld64bit"))
.settings(
  name := "assembler.examples.x86.helloWorld64bit",
  version := "1.0",
  coverageEnabled := false
).dependsOn(assembler, assemblerX86)

