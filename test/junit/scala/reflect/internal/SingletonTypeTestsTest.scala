package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class SingletonTypeTestsTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  @Test def classTestsAgreeWithIdentity(): Unit = {
    definitions.init()
    val samples = List[Type](
      NoType, NoPrefix, WildcardType, ErrorType,
      BoundedWildcardType(TypeBounds.empty),
      definitions.AnyTpe, definitions.ListClass.tpe, definitions.ListClass.typeConstructor,
      definitions.ListClass.thisType, TypeBounds.empty, NullaryMethodType(definitions.AnyTpe),
    )
    for (tp <- samples) {
      assertEquals(tp.toString, tp eq NoType, tp.isNoType)
      assertEquals(tp.toString, tp eq NoPrefix, tp.isNoPrefix)
      assertEquals(tp.toString, tp eq WildcardType, tp.isWildcardType)
      assertEquals(tp.toString, tp eq ErrorType, tp.isErrorType)
    }
    // isWildcard is the wider, virtual test
    assertTrue(BoundedWildcardType(TypeBounds.empty).isWildcard)
    assertFalse(BoundedWildcardType(TypeBounds.empty).isWildcardType)
  }
}
