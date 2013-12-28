package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MirrorsTest {
  @Test def rootCompanionsAreConnected(): Unit = {
    val cm = scala.reflect.runtime.currentMirror
    import cm._
    assertEquals("RootPackage.moduleClass == RootClass", RootClass, RootPackage.moduleClass)
    assertEquals("RootClass.module == RootPackage", RootPackage, RootClass.module)
    assertEquals("EmptyPackage.moduleClass == EmptyPackageClass", EmptyPackageClass, EmptyPackage.moduleClass)
    assertEquals("EmptyPackageClass.module == EmptyPackage", EmptyPackage, EmptyPackageClass.module)
  }
}