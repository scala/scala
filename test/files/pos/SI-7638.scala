package miniboxing.tests.compile

trait Ordering[@specialized(Int) A] {
  def eqv(x: Array[A], y: Array[A]): Boolean = false
}

trait ArrayVectorOrder[@specialized(Int) A] extends Ordering[A] {
  override def eqv(x: Array[A], y: Array[A]): Boolean = super.eqv(x, y)
}

object vectorOrder {
  implicit def arrayOrder[@specialized(Int) A]() =
  /*
   * Before applying patch:
   *
   *      while compiling: SI-7638.scala
   *         during phase: mixin
   *      library version: version 2.10.3-20130625-164027-d22e8d282c
   *     compiler version: version 2.10.3-20130627-153946-54cb6af7db
   *   reconstructed args:
   *
   *   last tree to typer: TypeTree(class Array)
   *               symbol: class Array in package scala (flags: final)
   *    symbol definition: final class Array[T >: ? <: ?] extends Object
   *                  tpe: Array[Int]
   *        symbol owners: class Array -> package scala
   *       context owners: anonymous class anon$1 -> package compile
   *
   * == Expanded type of tree ==
   *
   * TypeRef(
   *   TypeSymbol(final class Array[T >: ? <: ?] extends Object)
   *   args = List(TypeRef(TypeSymbol(final abstract class Int extends )))
   * )
   *
   * unhandled exception while transforming SI-7638.scala
   * error: uncaught exception during compilation: java.lang.UnsupportedOperationException
   * error: java.lang.UnsupportedOperationException: tail of empty list
   * 	at scala.collection.immutable.Nil$.tail(List.scala:339)
   * 	at scala.collection.immutable.Nil$.tail(List.scala:334)
   * 	at scala.tools.nsc.transform.Mixin$$anonfun$scala$tools$nsc$transform$Mixin$$rebindSuper$1.apply(Mixin.scala:123)
   * 	at scala.tools.nsc.transform.Mixin$$anonfun$scala$tools$nsc$transform$Mixin$$rebindSuper$1.apply(Mixin.scala:122)
   * 	at scala.reflect.internal.SymbolTable.atPhase(SymbolTable.scala:207)
   * 	at scala.reflect.internal.SymbolTable.afterPhase(SymbolTable.scala:216)
   * 	at scala.tools.nsc.Global.afterPickler(Global.scala:1104)
   * 	at scala.tools.nsc.transform.Mixin.scala$tools$nsc$transform$Mixin$$rebindSuper(Mixin.scala:122)
   * 	at scala.tools.nsc.transform.Mixin$$anonfun$scala$tools$nsc$transform$Mixin$$mixinTraitMembers$1$1.apply(Mixin.scala:339)
   * 	at scala.tools.nsc.transform.Mixin$$anonfun$scala$tools$nsc$transform$Mixin$$mixinTraitMembers$1$1.apply(Mixin.scala:292)
   */
    new ArrayVectorOrder[A] { }
}
