ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization := "dev.hshn"
ThisBuild / homepage     := Some(url("https://github.com/hshn/yoshi"))
ThisBuild / licenses     := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer("hshn", "Shota Hoshino", "sht.hshn@gmail.com", url("https://github.com/hshn"))
)
ThisBuild / description :=
  "A validation library for Scala 3 that parses untyped input into domain types"
ThisBuild / versionScheme := Some("early-semver")

ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Werror",
)

ThisBuild / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

val zio = "2.1.24"

lazy val root = (project in file(".") withId "yoshi")
  .settings(
    Compile / unmanagedSourceDirectories   := Nil,
    Compile / unmanagedResourceDirectories := Nil,
    Test / unmanagedSourceDirectories      := Nil,
    Test / unmanagedResourceDirectories    := Nil,
    publish / skip                         := true,
  )
  .aggregate(
    core,
    `zio-prelude`,
  )

lazy val core = (project in file("core"))
  .settings(
    name := "yoshi",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"          % zio % Test,
      "dev.zio" %% "zio-test-sbt"      % zio % Test,
      "dev.zio" %% "zio-test-magnolia" % zio % Test,
    ),
  )

lazy val `zio-prelude` = (project in file("zio-prelude") withId "yoshi-zio-prelude")
  .dependsOn(core % "test->test;compile->compile")
  .settings(
    name := "yoshi-zio-prelude",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-prelude" % "1.0.0-RC46",
    ),
  )

lazy val docs = project
  .in(file("docs"))
  .dependsOn(core, `zio-prelude`)
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
  .settings(
    moduleName := "yoshi-docs",
    mdocIn := baseDirectory.value / "mdoc",
    mdocOut := (ThisBuild / baseDirectory).value / "website" / "docs",
    mdocVariables := Map("VERSION" -> version.value),
    scalacOptions -= "-Werror",
    publish / skip := true,
  )
