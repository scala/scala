package scala.reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  Traditionally macro implementations are defined as methods,
 *  but this trait provides an alternative way of encoding macro impls as
 *  bundles, traits which extend `scala.reflect.macros.BlackboxMacro` or`scala.reflect.macros.WhiteboxMacro` .
 *
 *  Instead of:
 *
 *    def impl[T: c.WeakTypeTag](c: BlackboxContext)(x: c.Expr[Int]) = ...
 *
 *  One can write:
 *
 *    trait Impl extends BlackboxMacro {
 *      def apply[T: c.WeakTypeTag](x: c.Expr[Int]) = ...
 *    }
 *
 *  Without changing anything else at all.
 *
 *  This language feature is useful in itself in cases when macro implementations
 *  are complex and need to be modularized. State of the art technique of addressing this need is quite heavyweight:
 *  http://docs.scala-lang.org/overviews/macros/overview.html#writing_bigger_macros.
 *
 *  @see `scala.reflect.macros.WhiteboxMacro`
 */
trait BlackboxMacro {
  /** The context to be used by the macro implementation.
   *
   *  Vanilla macro implementations have to carry it in their signatures, however when a macro is a full-fledged module,
   *  it can define the context next to the implementation, makes implementation signature more lightweight.
   */
  val c: BlackboxContext
}
