ThisBuild / scalaVersion := "2.13.11"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked",
  "-feature",
)

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
      "org.typelevel" %% "cats-core"         % "2.9.0",
      "dev.zio"       %% "zio"               % "2.0.2",
      "dev.zio"       %% "zio-prelude"       % "1.0.0-RC16",
      "dev.zio"       %% "zio-test"          % "2.0.2" % Test,
      "dev.zio"       %% "zio-test-sbt"      % "2.0.2" % Test,
      "dev.zio"       %% "zio-test-magnolia" % "2.0.2" % Test,
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    Compile / sourceGenerators += task[Seq[File]] {
      val directory = (Compile / sourceManaged).value / "okay" / "syntax"

      val source = TupleOperationGenerator.generateZValidatedSyntax
      val file   = directory / s"TupleZValidationSyntax.scala"

      IO.write(
        file,
        source,
      )

      file :: Nil
    },
  )
