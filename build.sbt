def zioVersion = "1.0.0-RC19"
def zioNioVersion = "1.0.0-RC6"

lazy val root = (project in file("."))
  .settings(
    name := "hpack",
    scalaVersion := "2.13.2",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-nio"      % zioNioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
