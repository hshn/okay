ThisBuild / scalaVersion := "3.3.7"
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
  )
  .aggregate(
    core,
    `zio-prelude`,
  )

lazy val core = (project in file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"          % zio % Test,
      "dev.zio" %% "zio-test-sbt"      % zio % Test,
      "dev.zio" %% "zio-test-magnolia" % zio % Test,
    ),
  )

lazy val `zio-prelude` = (project in file("zio-prelude") withId "yoshi-zio-prelude")
  .dependsOn(core % "test->test;compile->compile")
  .settings(
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
