package scala.tools.nsc

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.classpath.DefaultFlatClassPathManager

/** Sample JUnit test that shows that all pieces
    of JUnit infrastructure work correctly */
@RunWith(classOf[JUnit4])
class FlatClassPathSampleTest {
  @Test
  def testFlatClassPath: Unit = {
    val settings = new Settings
    settings.usejavacp.value = true
    val classpath = DefaultFlatClassPathManager.createClassPath(settings)
    val topLevelPackages = classpath.packages("")
    assertTrue(topLevelPackages.nonEmpty)
    val classesInScalaPackage = classpath.classes("scala")
    assertTrue(classesInScalaPackage.nonEmpty)
  }
}
