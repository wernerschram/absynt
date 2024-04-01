inThisBuild(
  List(
    scalaVersion := "3.4.1",
    organization := "org.werner",
    licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
    semanticdbEnabled := true,
    scalacOptions ++= Seq(
      "-feature",
      "-new-syntax",
      "-source", "3.4",
      "-unchecked",
      "-deprecation",
    )
  )
)

autoCompilerPlugins := true

val testVersion = "3.2.9"
val spec2Version = "4.7.0"

git.useGitDescribe := true

lazy val root = (project in file("."))
.settings(
  publish/skip:= true,
).aggregate(
  absynt,
  absyntInProc,
  absyntX86,
  absyntARM,
  ARMBootRpiExample,
  X86BootFlagExample,
  X86HelloWorld32Example,
  X86HelloWorld64Example
)

lazy val absynt = (project in file("absynt"))
.enablePlugins(GitVersioning)
.enablePlugins(BuildInfoPlugin)
.settings(
  name := "absynt",
  publishMavenStyle := false,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
)

lazy val absyntInProc = (project in file("absynt.inproc"))
.enablePlugins(GitVersioning)
.enablePlugins(BuildInfoPlugin)
.settings(
  name := "absynt.inproc",
  publishMavenStyle := false,
  libraryDependencies += "net.java.dev.jna" % "jna" % "5.3.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val absyntX86 = (project in file("absynt.x86"))
.enablePlugins(GitVersioning)
.enablePlugins(BuildInfoPlugin)
.settings(
  name := "absynt.x86",
  publishMavenStyle := false,
//  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val absyntARM = (project in file("absynt.arm"))
.enablePlugins(GitVersioning)
.enablePlugins(BuildInfoPlugin)
.settings(
  name := "absynt.arm",
  publishMavenStyle := false,
  libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % Test,
).dependsOn(absynt)

lazy val ARMBootRpiExample = (project in file("examples/arm/bootRpi")) 
.settings(
  name := "absynt.examples.arm.bootRpi",
  coverageEnabled := false,
  publish/skip:= true,
).dependsOn(absynt, absyntARM)

lazy val X86BootFlagExample = (project in file("examples/x86/bootFlag"))
.settings(
  name := "absynt.examples.x86.bootFlag",
  coverageEnabled := false,
  publish/skip:= true,
).dependsOn(absynt, absyntX86)

lazy val X86HelloWorld32Example = (project in file("examples/x86/helloWorld32bit"))
.settings(
  name := "absynt.examples.x86.helloWorld32bit",
  coverageEnabled := false,
  publish/skip:= true,
).dependsOn(absynt, absyntX86)

lazy val X86HelloWorld64Example = (project in file("examples/x86/helloWorld64bit"))
.settings(
  name := "absynt.examples.x86.helloWorld64bit",
  coverageEnabled := false,
  publish/skip:= true,
).dependsOn(absynt, absyntX86)

lazy val X86InProc64Example = (project in file("examples/x86/InProc64bit"))
.settings(
  name := "absynt.examples.x86.inProc64bit",
  coverageEnabled := false,
  publish/skip:= true,
).dependsOn(absynt, absyntX86, absyntInProc)

