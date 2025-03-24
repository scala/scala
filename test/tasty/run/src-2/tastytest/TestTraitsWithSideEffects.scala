package tastytest

import collection.mutable

object TestTraitsWithSideEffects extends Suite("TestTraitsWithSideEffects") {
  import TraitsWithSideEffects._

  def checkEntries(map: mutable.Map[String, Boolean]): Boolean =
    map.getOrElse("True", false) && !map.getOrElse("False", true)

  test("init map and mixin field") {
    val exprMap = new ExprMap {}
    assert(checkEntries(exprMap.map))
  }

  test("init map with no backing field") {
    class ExprMapInit(val map: mutable.Map[String, Boolean]) extends ExprMapNoField
    val exprMap = new ExprMapInit(mutable.HashMap.empty)
    assert(checkEntries(exprMap.map))
  }
}
