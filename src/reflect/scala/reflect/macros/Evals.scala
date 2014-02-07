package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that provides
 *  a facility to evaluate trees.
 */
trait Evals {
  self: blackbox.Context =>

  /** Takes a typed wrapper for a tree of type `T` and evaluates it to a value of type `T`.
   *
   *  Can be used to perform compile-time computations on macro arguments to the extent
   *  permitted by the shape of the arguments.
   *
   *  Known issues: because of [[https://issues.scala-lang.org/browse/SI-5748 https://issues.scala-lang.org/browse/SI-5748]]
   *  trees being evaluated first need to undergo `untypecheck`. Resetting symbols and types
   *  mutates the tree in place, therefore the conventional approach is to `duplicate` the tree first.
   *
   *  {{{
   *  scala> def impl(c: Context)(x: c.Expr[String]) = {
   *       | val x1 = c.Expr[String](c.untypecheck(x.tree.duplicate))
   *       | println(s"compile-time value is: ${c.eval(x1)}")
   *       | x
   *       | }
   *  impl: (c: Context)(x: c.Expr[String])c.Expr[String]
   *
   *  scala> def test(x: String) = macro impl
   *  test: (x: String)String
   *
   *  scala> test("x")
   *  compile-time value is: x
   *  res0: String = x
   *
   *  scala> test("x" + "y")
   *  compile-time value is: xy
   *  res1: String = xy
   *
   *  scala> val x = "x"
   *  x: String = x
   *
   *  scala> test(x + "y")
   *  compile-time value is: xy
   *  res2: String = xy
   *
   *  scala> { val x = "x"; test(x + "y") }
   *  error: exception during macro expansion:
   *  scala.tools.reflect.ToolBoxError: reflective compilation failed
   *  }}}
   *
   *  Note that in the last case evaluation has failed, because the argument of a macro
   *  refers to a runtime value `x`, which is unknown at compile time.
   */
  def eval[T](expr: Expr[T]): T
}
