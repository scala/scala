package scala.build

import sbt._, Keys._

object TestJarSize {
  final private case class JarSize(currentBytes: Long, errorThreshold: Double, warnThreshold: Double)

  private val libraryJarSize = JarSize(5525657L, 1.03, 1.015)
  private val reflectJarSize = JarSize(3531794L, 1.03, 1.015)

  val testJarSizeImpl: Def.Initialize[Task[Unit]] = Def.task {
    testJarSize1("library", libraryJarSize).value
    testJarSize1("reflect", reflectJarSize).value
  }

  private def testJarSize1(projectId: String, jarSize: JarSize): Def.Initialize[Task[Unit]] = Def.task {
    import jarSize._
    val log = state.value.log
    val jar = (packageBin in Compile in LocalProject(projectId)).value
    val actualBytes = jar.length()
    if (actualBytes > (currentBytes * errorThreshold)) {
      fail(s"The $projectId jar is too big: $actualBytes bytes.")
    } else if (actualBytes > (currentBytes * warnThreshold)) {
      val percent = (actualBytes - currentBytes).toDouble / currentBytes.toDouble
      log.warn(s"The $projectId jar is getting too big: $actualBytes bytes or $percent% larger.")
    }
  }

  private def fail(message: String): Nothing = {
    val fail = new MessageOnlyException(message)
    fail.setStackTrace(new Array[StackTraceElement](0))
    throw fail
  }
}
