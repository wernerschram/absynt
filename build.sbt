scalaVersion in ThisBuild := "2.13.0"

val testVersion = "3.0.8"
val spec2Version = "4.7.0"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")
resolvers += Resolver.bintrayRepo("wernerschram", "absynt")

lazy val root = (project in file("."))
.settings(
  skip in publish := true,
).aggregate(
  absynt,
  absyntX86,
  absyntARM,
  ARMBootRpiExample,
  X86BootFlagExample,
  X86HelloWorld32Example,
  X86HelloWorld64Example
)

lazy val absynt = (project in file("absynt"))
.settings(
  name := "absynt",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  libraryDependencies += "org.specs2" %% "specs2-core" % spec2Version % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
)

lazy val absyntInProc = (project in file("absynt.inproc"))
.settings(
  name := "absynt.inproc",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  libraryDependencies += "net.java.dev.jna" % "jna" % "5.3.1",
  libraryDependencies += "org.specs2" %% "specs2-core" % spec2Version % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val absyntX86 = (project in file("absynt.x86"))
.settings(
  name := "absynt.x86",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  libraryDependencies += "org.specs2" %% "specs2-core" % spec2Version % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val absyntARM = (project in file("absynt.arm"))
.settings(
  name := "absynt.arm",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  libraryDependencies += "org.specs2" %% "specs2-core" % spec2Version % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val ARMBootRpiExample = (project in file("examples/arm/bootRpi")) 
.settings(
  name := "absynt.examples.arm.bootRpi",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  coverageEnabled := false,
  skip in publish := true,
).dependsOn(absynt, absyntARM)

lazy val X86BootFlagExample = (project in file("examples/x86/bootFlag"))
.settings(
  name := "absynt.examples.x86.bootFlag",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  coverageEnabled := false,
  skip in publish := true,
).dependsOn(absynt, absyntX86)

lazy val X86HelloWorld32Example = (project in file("examples/x86/helloWorld32bit"))
.settings(
  name := "absynt.examples.x86.helloWorld32bit",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  coverageEnabled := false,
  skip in publish := true,
).dependsOn(absynt, absyntX86)

lazy val X86HelloWorld64Example = (project in file("examples/x86/helloWorld64bit"))
.settings(
  name := "absynt.examples.x86.helloWorld64bit",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  coverageEnabled := false,
  skip in publish := true,
).dependsOn(absynt, absyntX86)

lazy val X86InProc64Example = (project in file("examples/x86/InProc64bit"))
.settings(
  name := "absynt.examples.x86.inProc64bit",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
  coverageEnabled := false,
  skip in publish := true,
).dependsOn(absynt, absyntX86, absyntInProc)

