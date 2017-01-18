organization in ThisBuild := "org.scala-lang"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.1"

scalacOptions in ThisBuild ++=
  Seq("-deprecation", "-unchecked", "-Yno-imports", "-language:higherKinds")

testOptions in ThisBuild += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")

fork in Test := true

parallelExecution in Test := false

val collections =
  project.in(file("."))
    .settings(
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0",
        "com.novocode" % "junit-interface" % "0.11" % Test
      )
    )

val timeBenchmark =
  project.in(file("benchmarks/time"))
    .dependsOn(collections)
    .enablePlugins(JmhPlugin)

val memoryBenchmark =
  project.in(file("benchmarks/memory"))
    .dependsOn(collections)