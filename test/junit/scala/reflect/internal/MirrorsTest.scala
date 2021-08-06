package scala.reflect.internal

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class MirrorsTest {
  @Test def rootCompanionsAreConnected(): Unit = {
    val cm = scala.reflect.runtime.currentMirror
    import cm._
    assertEquals(RootClass, RootPackage.moduleClass, "RootPackage.moduleClass == RootClass")
    assertEquals(RootPackage, RootClass.module, "RootClass.module == RootPackage")
    assertEquals(EmptyPackageClass, EmptyPackage.moduleClass, "EmptyPackage.moduleClass == EmptyPackageClass")
    assertEquals(EmptyPackage, EmptyPackageClass.module, "EmptyPackageClass.module == EmptyPackage")
  }
}
