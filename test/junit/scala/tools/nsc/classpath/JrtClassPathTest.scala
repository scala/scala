/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.nsc.{CloseableRegistry, Settings}
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.nsc.util.ClassPath
import scala.tools.util.PathResolver

class JrtClassPathTest {

  @Test def lookupJavaClasses(): Unit = {
    val specVersion = scala.util.Properties.javaSpecVersion
    // Run the test using the JDK8 or 9 provider for rt.jar depending on the platform the test is running on.
    val closeableRegistry = new CloseableRegistry
    val cp: ClassPath =
      if (specVersion == "" || specVersion == "1.8") {
        val settings = new Settings()
        val resolver = new PathResolver(settings, closeableRegistry)
        val elements = new ClassPathFactory(settings, closeableRegistry).classesInPath(resolver.Calculated.javaBootClassPath)
        AggregateClassPath(elements)
      }
      else JrtClassPath(None, closeableRegistry).get

    assertEquals(Nil, cp.classes(""))
    assertTrue(cp.packages("java").exists(_.name == "java.lang"), cp.packages("java").toString)
    assertTrue(cp.classes("java.lang").exists(_.name == "Object"))
    val jl_Object = cp.classes("java.lang").find(_.name == "Object").get
    assertEquals("java/lang/Object", AsmUtils.classFromBytes(jl_Object.file.toByteArray).name)
    assertTrue(cp.list("java.lang").packages.exists(_.name == "java.lang.annotation"))
    assertTrue(cp.list("java.lang").classesAndSources.exists(_.name == "Object"))
    assertTrue(cp.findClass("java.lang.Object").isDefined)
    assertTrue(cp.findClassFile("java.lang.Object").isDefined)

    closeableRegistry.close()
  }
}
