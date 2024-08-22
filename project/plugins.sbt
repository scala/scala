scalacOptions ++= Seq(
  "-unchecked",
  "-feature",
  "-deprecation",
  "-Xlint:-unused,_",
  "-Werror",
  "-Wconf:msg=IntegrationTest .* is deprecated:s,msg=itSettings .* is deprecated:s")

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.16.0"

libraryDependencies += "biz.aQute.bnd" % "biz.aQute.bndlib" % "6.1.0"

enablePlugins(BuildInfoPlugin)

// configure sbt-buildinfo to send the externalDependencyClasspath to the main build, which allows using it for the IntelliJ project config

lazy val buildClasspath = taskKey[String]("Colon-separated (or semicolon-separated in case of Windows) list of entries on the sbt build classpath.")

buildClasspath := (Compile / externalDependencyClasspath).value.map(_.data).mkString(java.io.File.pathSeparator)

buildInfoKeys := Seq[BuildInfoKey](buildClasspath)

buildInfoPackage := "scalabuild"

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")

libraryDependencies ++= Seq(
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.11.9.201909030838-r",
  "org.slf4j" % "slf4j-nop" % "2.0.0",
  "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
)

Global / concurrentRestrictions := Seq(
  Tags.limitAll(1) // workaround for https://github.com/sbt/sbt/issues/2970
)

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.10.0")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")

addSbtPlugin("com.gradle" % "sbt-develocity" % "1.1.1")
