scalaVersion in ThisBuild := "2.13.0"

val testVersion = "3.0.8"
val spec2Version = "4.7.0"

lazy val root = project in file(".") aggregate(
  absynt,
  absyntX86,
  absyntARM,
  ARMBootRpiExample,
  X86BootFlagExample,
  X86HelloWorld32Example,
  X86HelloWorld64Example
)

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

lazy val absynt = (project in file("absynt"))
.settings(
  name := "absynt",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % spec2Version % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
)

lazy val absyntInProc = (project in file("absynt.inproc"))
.settings(
  name := "absynt.inproc",
  version := "1.0",
  libraryDependencies += "net.java.dev.jna" % "jna" % "5.3.1",
  libraryDependencies += "org.specs2" %% "specs2-core" % spec2Version % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val absyntX86 = (project in file("absynt.x86"))
.settings(
  name := "absynt.x86",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % spec2Version % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val absyntARM = (project in file("absynt.arm"))
.settings(
  name := "absynt.arm",
  version := "1.0",
  libraryDependencies += "org.specs2" %% "specs2-core" % spec2Version % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val ARMBootRpiExample = (project in file("examples/arm/bootRpi")) 
.settings(
  name := "absynt.examples.arm.bootRpi",
  version := "1.0",
  coverageEnabled := false
).dependsOn(absynt, absyntARM)

lazy val X86BootFlagExample = (project in file("examples/x86/bootFlag"))
.settings(
  name := "absynt.examples.x86.bootFlag",
  version := "1.0",
  coverageEnabled := false
).dependsOn(absynt, absyntX86)

lazy val X86HelloWorld32Example = (project in file("examples/x86/helloWorld32bit"))
.settings(
  name := "absynt.examples.x86.helloWorld32bit",
  version := "1.0",
  coverageEnabled := false
).dependsOn(absynt, absyntX86)

lazy val X86HelloWorld64Example = (project in file("examples/x86/helloWorld64bit"))
.settings(
  name := "absynt.examples.x86.helloWorld64bit",
  version := "1.0",
  coverageEnabled := false
).dependsOn(absynt, absyntX86)

lazy val X86InProc64Example = (project in file("examples/x86/InProc64bit"))
.settings(
  name := "absynt.examples.x86.inProc64bit",
  version := "1.0",
  coverageEnabled := false
).dependsOn(absynt, absyntX86, absyntInProc)

