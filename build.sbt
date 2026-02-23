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

lazy val root = (project in file(".") withId "okay")
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
      val zio = "2.1.24"

      Seq(
        "org.typelevel" %% "cats-core"         % "2.13.0",
        "dev.zio"       %% "zio"               % zio,
        "dev.zio"       %% "zio-prelude"       % "1.0.0-RC46",
        "dev.zio"       %% "zio-test"          % zio % Test,
        "dev.zio"       %% "zio-test-sbt"      % zio % Test,
        "dev.zio"       %% "zio-test-magnolia" % zio % Test,
      )
    },
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
