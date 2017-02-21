organization in ThisBuild := "ch.epfl.scala"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.1"

scalacOptions in ThisBuild ++=
  Seq("-deprecation", "-unchecked", "-Yno-imports", "-language:higherKinds")

testOptions in ThisBuild += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")

fork in Test in ThisBuild := true

parallelExecution in Test in ThisBuild := false

val collections =
  project.in(file("."))
    .settings(
      name := "collection-strawman",
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0",
        "com.novocode" % "junit-interface" % "0.11" % Test
      ),
      credentials ++= (
        for {
          username <- sys.env.get("SONATYPE_USERNAME")
          password <- sys.env.get("SONATYPE_PASSWORD")
        } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
      ).toList
    )

val timeBenchmark =
  project.in(file("benchmarks/time"))
    .dependsOn(collections)
    .enablePlugins(JmhPlugin)
    .settings(
      charts := Def.inputTaskDyn {
        val benchmarks = Def.spaceDelimited().parsed
        val targetDir = crossTarget.value
        val jmhReport = targetDir / "jmh-result.json"
        val runTask = run in Jmh
        Def.inputTask {
          val _ = runTask.evaluated
          strawman.collection.Bencharts(jmhReport, "Execution time (lower is better)", targetDir)
          targetDir
        }.toTask(s" -rf json -rff ${jmhReport.absolutePath} ${benchmarks.mkString(" ")}")
      }.evaluated
    )

val memoryBenchmark =
  project.in(file("benchmarks/memory"))
    .dependsOn(collections)
    .settings(
      libraryDependencies += "org.spire-math" %% "jawn-ast" % "0.10.4",
      charts := Def.inputTaskDyn {
        val targetDir = crossTarget.value
        val report = targetDir / "report.json"
        val runTask = run in Compile
        Def.inputTask {
          val _ = runTask.evaluated
          strawman.collection.Bencharts(report, "Memory footprint (lower is better)", targetDir)
          targetDir
        }.toTask(s" ${report.absolutePath}")
      }.evaluated
    )

lazy val charts = inputKey[File]("Runs the benchmarks and produce charts")