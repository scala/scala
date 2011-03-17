import sbt._

/** I'm sure much of this is done the hard way, but it's done!
 */
class JlineProject(info: ProjectInfo) extends DefaultProject(info) with ProguardProject {
  val snapShots = "Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val jansi = "org.fusesource.jansi" % "jansi" % "1.4"
  val junitInterface = "com.novocode" % "junit-interface" % "0.5" % "test->default"

  // val junit = "junit" % "junit" % "4.8.1" % "test"
  // lazy val jansiPath = (managedDependencyPath / "compile" ** "jansi*").get.toList.head.absolutePath

  override def javaCompileOptions = super.javaCompileOptions ++ javaCompileOptions("-target", "1.5")

  override def makeInJarFilter(file: String) =  {
    if (!file.startsWith("jansi")) super.makeInJarFilter(file)
    else List(
      "!META-INF/MANIFEST.MF",
      "org/fusesource/hawtjni/runtime",
      "org/fusesource/hawtjni/runtime/Callback.class",
      "org/fusesource/hawtjni/runtime/Library.class",
      "!org/fusesource/hawtjni/**",
      "!META-INF/maven/org.fusesource.hawtjni",
      "!META-INF/maven/org.fusesource.jansi",
      "!META-INF/maven/org.fusesource.hawtjni/**",
      "!META-INF/maven/org.fusesource.jansi/**"
    ) mkString ", "
  }

  override def proguardOptions = List(
    "-dontshrink",
    "-keep class *",
    "-keepdirectories"
  )
}
