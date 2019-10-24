scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation",
  "-Xlint:-unused,_", "-Xfatal-warnings")

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"

libraryDependencies += "biz.aQute.bnd" % "biz.aQute.bnd" % "2.4.1"

enablePlugins(BuildInfoPlugin)

// configure sbt-buildinfo to send the externalDependencyClasspath to the main build, which allows using it for the IntelliJ project config

lazy val buildClasspath = taskKey[String]("Colon-separated (or semicolon-separated in case of Windows) list of entries on the sbt build classpath.")

buildClasspath := (externalDependencyClasspath in Compile).value.map(_.data).mkString(java.io.File.pathSeparator)

buildInfoKeys := Seq[BuildInfoKey](buildClasspath)

buildInfoPackage := "scalabuild"

resolvers += Resolver.url("bintray",
  new java.net.URL("https://dl.bintray.com/typesafe/sbt-plugins"))(Resolver.defaultIvyPatterns)
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.3.0")

libraryDependencies ++= Seq(
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.6.0.201612231935-r",
  "org.slf4j" % "slf4j-nop" % "1.7.23",
  "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0"
)

concurrentRestrictions in Global := Seq(
  Tags.limitAll(1) // workaround for https://github.com/sbt/sbt/issues/2970
)

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.3")

scalaVersion := "2.12.7"

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.0.0")

// See DottySupport.scala
if (Option(System.getProperty("scala.build.compileWithDotty")).map(_.toBoolean).getOrElse(false))
  Seq(addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.3.3"))
else
  Seq()

addSbtPlugin("com.lightbend" % "sbt-whitesource" % "0.1.16")
