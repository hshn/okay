ThisBuild / scalaVersion := "2.13.16"
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
    libraryDependencies ++= {
      val zio = "2.0.21"

      Seq(
        "org.typelevel" %% "cats-core"         % "2.10.0",
        "dev.zio"       %% "zio"               % zio,
        "dev.zio"       %% "zio-prelude"       % "1.0.0-RC23",
        "dev.zio"       %% "zio-test"          % zio % Test,
        "dev.zio"       %% "zio-test-sbt"      % zio % Test,
        "dev.zio"       %% "zio-test-magnolia" % zio % Test,
      )
    },
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
