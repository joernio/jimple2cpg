name := "jimple2cpg"
organization := "io.joern"

scalaVersion := "2.13.6"

val cpgVersion       = "1.3.392"
val sootVersion      = "4.2.1"
val slf4jVersion     = "1.7.32"
val scalatestVersion = "3.2.9"

Test / fork := true
resolvers ++= Seq(
  Resolver.mavenLocal,
  "Atlassian Maven Repository" at "https://maven.atlassian.com/repository/public",
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public"
)

trapExit := false

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph" % cpgVersion,
  "io.shiftleft"  %% "semanticcpg"       % cpgVersion,
  "io.shiftleft"  %% "dataflowengineoss" % cpgVersion,
  "io.shiftleft"  %% "semanticcpg-tests" % cpgVersion       % Test classifier "tests",
  "org.soot-oss"   % "soot"              % sootVersion,
  "org.slf4j"      % "slf4j-api"         % slf4jVersion,
  "org.slf4j"      % "slf4j-simple"      % slf4jVersion,
  "org.scalatest" %% "scalatest"         % scalatestVersion % Test
)

sonatypeCredentialHost := "s01.oss.sonatype.org"
publishTo := sonatypePublishToBundle.value
scmInfo := Some(
  ScmInfo(url("https://github.com/joernio/jimple2cpg"), "scm:git@github.com:joernio/jimple2cpg.git")
)
homepage := Some(url("https://github.com/joernio/jimple2cpg/"))
licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
developers := List(
  Developer(
    "DavidBakerEffendi",
    "David Baker Effendi",
    "dbe@sun.ac.za",
    url("https://github.com/DavidBakerEffendi")
  ),
  Developer(
    "fabsx00",
    "Fabian Yamaguchi",
    "fabs@shiftleft.io",
    url("https://github.com/fabsx00")
  )
)

enablePlugins(JavaAppPackaging, GitVersioning, BuildInfoPlugin)

Global / onChangedBuildSource := ReloadOnSourceChanges
