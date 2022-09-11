ThisBuild / scalaVersion := "2.13.8"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    Compile / unmanagedSourceDirectories   := Nil,
    Compile / unmanagedResourceDirectories := Nil,
    Test / unmanagedSourceDirectories      := Nil,
    Test / unmanagedResourceDirectories    := Nil,
  )
  .aggregate(
    core,
  )

lazy val core = (project in file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"         % "2.8.0",
      "dev.zio"       %% "zio"               % "2.0.2",
      "dev.zio"       %% "zio-prelude"       % "1.0.0-RC15",
      "dev.zio"       %% "zio-test"          % "2.0.2"  % Test,
      "dev.zio"       %% "zio-test-sbt"      % "2.0.2"  % Test,
      "dev.zio"       %% "zio-test-magnolia" % "2.0.2"  % Test,
      "org.scalameta" %% "munit"             % "0.7.29" % Test,
      "org.scalameta" %% "munit-scalacheck"  % "0.7.29" % Test,
    ),
    scalacOptions += "-Yrangepos",
    testFrameworks += new TestFramework("munit.Framework"),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  )
