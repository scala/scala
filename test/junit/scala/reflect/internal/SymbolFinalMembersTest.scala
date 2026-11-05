package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class SymbolFinalMembersTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  // Reads false, and silently turns every cache into an allocation, if the flag is set after the mixins' initializers.
  @Test def reusableInstancesAreEnabledInACompilerUniverse(): Unit = {
    assertTrue(isCompilerUniverse)
    assertSame(findMemberInstance.using(x => x), findMemberInstance.using(x => x))
  }
}
