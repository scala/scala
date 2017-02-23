/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Predef.scala 21249 2010-03-24 15:37:50Z extempore $


package scala

/** The <code>EmbeddedControls</code> object provides method definitions
 *  where calls to the methods are treated by the compiler in a special way.
 *  The reason to express these calls as methods is to give embedded DSLs a chance
 *  to provide their own definitions and thereby override the standard
 *  interpretation of the compiler.
 *
 *  Example: When faces with an `if` construct, the parser will generate
 *
 *  a method call: __ifThenElse(cond, thenp, elsep)
 *
 *  This method call will be bound to an implementation based on normal rules of scoping.
 *  If it binds to the standard one in this trait, the type checker will
 *  replace it by an `If` tree node. If not, the call will be left as it is
 *  and a staging or interpreting DSL can take over.
 *
 * @NOTE: This is experimental.
 *        None of the above will happen unless you compile with -Yvirtualize.
 */
trait EmbeddedControls {
  /** Note why types are by-value
   */
  def __whileDo(cond: Boolean, body: Unit): Unit =
    throw new UnsupportedOperationException("__whileDo")

  def __doWhile(body: Unit, cond: Boolean): Unit =
    throw new UnsupportedOperationException("__doWhile")

  def __ifThenElse[T](cond: => Boolean, thenp: => T, elsep: => T): T =
    throw new UnsupportedOperationException("__ifThenElse")

  def __newVar[T](init: T): T =
    throw new UnsupportedOperationException("__newVar")

  def __assign[T](lhs: T, rhs: T): Unit =
    throw new UnsupportedOperationException("__assign")

  def __return(expr: Any): Nothing =
    throw new UnsupportedOperationException("__return")

  def __equal(expr1: Any, expr2: Any): Boolean =
    throw new UnsupportedOperationException("__equal")

  /** Struct is a marker trait used to indicate that `new C { ... }` should be reified.
   *
   * Selections on the result `e` of the reified object creation, `e.x_i`,
   * are treated specially, as outlined below.
   *
   * If there's a type constructor `Rep` (of kind `* -> *`) so that `C <:< Struct`,
   * the expression `new C { (val x_i: T_i = v_i)* }` is turned into
   * the call `__new(("x_i", (self_i: Rep[C{ (val x_i: T_i')* }]) => v_i')*)`,
   * which is typed with expected type `Rep[C{ (val x_i: T_i')* }]`
   *
   * For all i,
   *   - there must be some T_i' so that T_i = Rep[T_i'] -- or, if that previous equality is not unifiable, T_i = T_i'
   *   - the v_i' result from retyping v_i with expected type Rep[T_i'],
   *     after replacing `this` by a fresh variable `self_i` (with type `Rep[C{ (val x_i: T_i')* }]`)
   *
   * This assumes there is a method in scope similar to: `def __new[T](args: (String, Rep[T] => Rep[_])*): Rep[T]`
   *
   * When a selection `e.x_i` does not type check according to the normal typing rules,
   * and `e` has type `Rep[C{ (val x_i: T_i')* }]` (where `C` meets the criteria outlined above),
   * `e.x_i` is turned into `e.selectDynamic[T_i]("x_i")`
   */
  trait Struct

  /**
   * given `def OptiML[R](b: => R) = new Scope[OptiML, OptiMLExp, R](b)`
   *
   * `OptiML { body }` is expanded to:
   *
   *  trait DSLprog$ extends OptiML {
   *    def apply = body
   *  }
   *  (new DSLprog$ with OptiMLExp): OptiML with OptiMLExp
   *
   *
   */
  class Scope[Interface, Implementation, Result](body: => Result)
}

trait ProxyControlsBase extends EmbeddedControls {
  type TransparentProxy[+T]

  def __forward[A,B,C](self: TransparentProxy[A], method: String, x: TransparentProxy[B]*): TransparentProxy[C] =
    throw new UnsupportedOperationException("__forward")
}

trait ProxyControls extends ProxyControlsBase
