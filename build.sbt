val zioVersion = "1.0.0-RC19"
val zioNioVersion = "1.0.0-RC6"

lazy val root = (project in file("."))
  .settings(
    name := "hpack",
    scalaVersion := "2.13.2",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies ++= Seq(
      "dev.zio"        %% "zio-nio"                  % zioNioVersion,
      "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.21"     % "test",
      "dev.zio"        %% "zio-test"                 % zioVersion % "test",
      "dev.zio"        %% "zio-test-sbt"             % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

enablePlugins(JmhPlugin)
