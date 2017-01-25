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
    .settings(
      // runs the benchmarks and produce charts
      InputKey[Unit]("charts") := {
        val benchmarks = Def.spaceDelimited().parsed
        val targetDir = crossTarget.value / "bencharts"
        val jmhReport = targetDir / "jmh-result.json"
        val jmhArgs = s" -rf json -rff ${jmhReport.absolutePath} $benchmarks"
        // HACK We should use `jmhArgs` here
        val _ = (run in Jmh).partialInput(" -rf json -rff target/scala-2.12/bencharts/jmh-result.json").evaluated
        strawman.collection.Bencharts(jmhReport, targetDir)
      }
    )

val memoryBenchmark =
  project.in(file("benchmarks/memory"))
    .dependsOn(collections)