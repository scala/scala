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

  // A stub stands for a class missing from the classpath. Asking for its children must not report
  // the missing-classpath error, as it did not before the children were loaded on demand.
  @Test def childrenOfStubAreEmptyWithoutError(): Unit = {
    val stub = rootMirror.EmptyPackageClass.newStubSymbol(TypeName("Missing"), "missing")
    assertEquals(Set.empty, stub.children)
    assertEquals(Set.empty, stub.knownDirectSubclasses)
    assertFalse(reporter.infos.mkString("\n"), reporter.hasErrors)
  }
}
