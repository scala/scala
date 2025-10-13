scalacOptions ++= Seq(
  "-unchecked",
  "-feature",
  // "-deprecation",
  // "-Xlint:-unused,_",
  // "-Werror",
  "-Wconf:msg=IntegrationTest .* is deprecated:s,msg=itSettings .* is deprecated:s"
)

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.19.0"

libraryDependencies += "org.pantsbuild" % "jarjar" % "1.7.2"

libraryDependencies += "biz.aQute.bnd" % "biz.aQute.bndlib" % "6.1.0"

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")

libraryDependencies ++= Seq(
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.11.9.201909030838-r",
  "org.slf4j" % "slf4j-nop" % "1.7.36",
  "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0"
  )

concurrentRestrictions in Global := Seq(
  Tags.limitAll(1) // workaround for https://github.com/sbt/sbt/issues/2970
)

addSbtPlugin("com.github.sbt" % "sbt-header" % "5.11.0")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

addSbtPlugin("com.gradle" % "sbt-develocity" % "1.3.1")
