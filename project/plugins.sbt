libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"

libraryDependencies += "org.pantsbuild" % "jarjar" % "1.6.0"

libraryDependencies += "biz.aQute.bnd" % "biz.aQute.bnd" % "2.4.1"

enablePlugins(BuildInfoPlugin)

// configure sbt-buildinfo to send the externalDependencyClasspath to the main build, which allows using it for the IntelliJ project config

lazy val buildClasspath = taskKey[String]("Colon-separated list of entries on the sbt build classpath.")

buildClasspath := (externalDependencyClasspath in Compile).value.map(_.data).mkString(":")

buildInfoKeys := Seq[BuildInfoKey](buildClasspath)

buildInfoPackage := "scalabuild"
