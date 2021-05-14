package scala.reflect.internal

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class SubstMapTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  // compile-test for https://github.com/scala/community-build/pull/1413
  new SubstMap[String](Nil, Nil) {
    protected def toType(fromtp: Type, tp: String) = fromtp
  }
}
