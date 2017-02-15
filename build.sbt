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
        val targetDir = crossTarget.value
        val jmhReport = targetDir / "jmh-result.json"
        val jmhArgs = s" -rf json -rff ${jmhReport.absolutePath} $benchmarks"
        // HACK We should use `jmhArgs` here
        val _ = (run in Jmh).partialInput(" -rf json -rff target/scala-2.12/jmh-result.json").evaluated
        strawman.collection.Bencharts(jmhReport, "Execution time (lower is better)", targetDir)
      }
    )

val memoryBenchmark =
  project.in(file("benchmarks/memory"))
    .dependsOn(collections)
    .settings(
      libraryDependencies += "org.spire-math" %% "jawn-ast" % "0.10.4",
      InputKey[Unit]("charts") := {
        val targetDir = crossTarget.value
        val report = targetDir / "report.json"
        // HACK We should use `report.absolutePath` here
        val _ = (run in Compile).fullInput(" benchmarks/memory/target/scala-2.12/report.json").evaluated
        strawman.collection.Bencharts(report, "Memory footprint (lower is better)", targetDir)
      }
    )