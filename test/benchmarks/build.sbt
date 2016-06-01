scalaHome := Some(file("../../build/pack"))
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-feature", "-Yopt:l:classpath")

lazy val root = (project in file(".")).
  enablePlugins(JmhPlugin).
  settings(
    name := "test-benchmarks",
    version := "0.0.1",
	libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.4"
  )
