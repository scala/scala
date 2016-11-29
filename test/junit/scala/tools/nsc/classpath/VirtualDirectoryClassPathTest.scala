/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.io.VirtualDirectory


@RunWith(classOf[JUnit4])
class VirtualDirectoryClassPathTest {

  @Test
  def virtualDirectoryClassPath_findClassFile(): Unit = {
    val base = new VirtualDirectory("base", None)
    val p1 = base subdirectoryNamed "p1"
    val p1_Test_class = p1.fileNamed("Test.class")
    val p2 = base subdirectoryNamed "p2"
    val p3 = p2 subdirectoryNamed "p3"
    val p4 = p3 subdirectoryNamed "p4"
    val p4_Test1_class = p4.fileNamed("Test.class")
    val classPath = VirtualDirectoryClassPath(base)

    assertEquals(Some(p1_Test_class), classPath.findClassFile("p1/Test"))

    assertEquals(None, classPath.findClassFile("p1/DoesNotExist"))
    assertEquals(None, classPath.findClassFile("DoesNotExist"))
    assertEquals(None, classPath.findClassFile("p2"))
    assertEquals(None, classPath.findClassFile("p2/DoesNotExist"))
    assertEquals(None, classPath.findClassFile("p4/DoesNotExist"))

    assertEquals(List("p1", "p2"), classPath.packages("").toList.map(_.name).sorted)
    assertEquals(List(), classPath.packages("p1").toList.map(_.name).sorted)
    assertEquals(List("p2.p3"), classPath.packages("p2").toList.map(_.name).sorted)
    assertEquals(List("p2.p3.p4"), classPath.packages("p2.p3").toList.map(_.name).sorted)
  }
}
