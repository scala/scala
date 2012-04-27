class Test {
  // m.$minus(1)
  // at scala.Predef$.assert(Predef.scala:185)
  // at scala.tools.nsc.Global.assert(Global.scala:187)
  // at scala.tools.nsc.typechecker.SuperAccessors$SuperAccTransformer.transform(SuperAccessors.scala:291)
  val a: (Map[Int, Int] => (Any => Any)) = { m => { case _ => m - 1} }

  // patmat-crash.scala:9: error: erroneous or inaccessible type
  val b: (Int => (Any => Any)) = { m => { case _ => m } }

  // no-symbol does not have an owner (this is a bug: scala version 2.10.0-20120420-170445-56c1f29250)
  // at scala.reflect.internal.SymbolTable.abort(SymbolTable.scala:45)
  // at scala.tools.nsc.Global.abort(Global.scala:202)
  // at scala.reflect.internal.Symbols$NoSymbol.owner(Symbols.scala:3031)
  // at scala.tools.nsc.typechecker.SuperAccessors$SuperAccTransformer.hostForAccessorOf(SuperAccessors.scala:474)
  // at scala.tools.nsc.typechecker.SuperAccessors$SuperAccTransformer.needsProtectedAccessor(SuperAccessors.scala:457)
  val c: (Int => (Any => Any)) = { m => { case _ => m.toInt } }
}