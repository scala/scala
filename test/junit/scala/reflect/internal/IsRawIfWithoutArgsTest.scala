package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class IsRawIfWithoutArgsTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  // scala/bug#400: JAVA is only set once the classfile is read, so an uninitialized class must not be answered from the flag
  @Test def uninitializedJavaClasses(): Unit = {
    definitions.init()
    val generic = rootMirror.getRequiredClass("java.util.concurrent.Exchanger")
    assertFalse(generic.isInitialized)
    assertTrue(isRawIfWithoutArgs(generic))
    val monomorphic = rootMirror.getRequiredClass("java.util.concurrent.Phaser")
    assertFalse(monomorphic.isInitialized)
    assertFalse(isRawIfWithoutArgs(monomorphic))
  }

  @Test def initializedClasses(): Unit = {
    definitions.init()
    assertFalse(isRawIfWithoutArgs(definitions.ListClass.initialize))
    assertTrue(isRawIfWithoutArgs(rootMirror.getRequiredClass("java.util.ArrayList").initialize))
    assertFalse(isRawIfWithoutArgs(definitions.StringClass.initialize))
  }
}
