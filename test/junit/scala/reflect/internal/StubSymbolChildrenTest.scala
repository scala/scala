package scala.reflect.internal

import org.junit.Assert.{assertEquals, assertFalse}
import org.junit.Test

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class StubSymbolChildrenTest {
  object symbolTable extends SymbolTableForUnitTesting {
    override val reporter = new StoreReporter(settings)
  }
  import symbolTable._

  // Asking for the children of a class missing from the classpath must not report it as missing.
  @Test def childrenOfStubAreEmptyWithoutError(): Unit = {
    val stub = rootMirror.EmptyPackageClass.newStubSymbol(TypeName("Missing"), "missing")
    assertEquals(Set.empty, stub.children)
    assertEquals(Set.empty, stub.knownDirectSubclasses)
    assertFalse(reporter.infos.mkString("\n"), reporter.hasErrors)
  }
}
