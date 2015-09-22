package scala
package reflect
package macros
package blackbox

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  The blackbox Scala macros context.
 *
 *  See [[scala.reflect.macros.package the overview page]] for a description of how macros work. This documentation
 *  entry provides information on the API available to macro writers.
 *
 *  A macro context wraps a compiler universe exposed in `universe` and having type [[scala.reflect.macros.Universe]].
 *  This type is a refinement over the generic reflection API provided in [[scala.reflect.api.Universe]]. The
 *  extended Universe provides mutability for reflection artifacts (e.g. macros can change types of compiler trees,
 *  add annotation to symbols representing definitions, etc) and exposes some internal compiler functionality
 *  such as `Symbol.deSkolemize` or `Tree.attachments`.
 *
 *  Another fundamental part of a macro context is `macroApplication`, which provides access to the tree undergoing
 *  macro expansion. Parts of this tree can be found in arguments of the corresponding macro implementations and
 *  in `prefix`, but `macroApplication` gives the full picture.
 *
 *  Other than that, macro contexts provide facilities for typechecking, exploring the compiler's symbol table and
 *  enclosing trees and compilation units, evaluating trees, logging warnings/errors and much more.
 *  Refer to the documentation of top-level traits in this package to learn the details.
 *
 *  If a macro def refers to a macro impl that uses `blackbox.Context`, then this macro def becomes a blackbox macro,
 *  which means that its expansion will be upcast to its return type, enforcing faithfullness of that macro to its
 *  type signature. Whitebox macros, i.e. the ones defined with `whitebox.Context`, aren't bound by this restriction,
 *  which enables a number of important use cases, but they are also going to enjoy less support than blackbox macros,
 *  so choose wisely. See the [[http://docs.scala-lang.org/overviews/macros/overview.html Macros Guide]] for more information.
 *
 *  @see `scala.reflect.macros.whitebox.Context`
 */
trait Context extends Aliases
                 with Enclosures
                 with Names
                 with Reifiers
                 with FrontEnds
                 with Infrastructure
                 with Typers
                 with Parsers
                 with Evals
                 with ExprUtils
                 with Internals {

  /** The compile-time universe. */
  val universe: Universe

  /** The mirror of the compile-time universe. */
  val mirror: universe.Mirror

  /** The type of the prefix tree from which the macro is selected.
   *  See the documentation entry for `prefix` for an example.
   */
  type PrefixType

  /** The prefix tree from which the macro is selected.
   *
   *  For example, for a macro `filter` defined as an instance method on a collection `Coll`,
   *  `prefix` represents an equivalent of `this` for normal instance methods:
   *
   *  {{{
   *  scala> class Coll[T] {
   *       | def filter(p: T => Boolean): Coll[T] = macro M.filter[T]
   *       | }; object M {
   *       | def filter[T](c: Context { type PrefixType = Coll[T] })
   *       |              (p: c.Expr[T => Boolean]): c.Expr[Coll[T]] =
   *       |   {
   *       |     println(c.prefix.tree)
   *       |     c.prefix
   *       |   }
   *       | }
   *  defined class Coll
   *  defined module Macros
   *
   *  scala> new Coll[Int]().filter(_ % 2 == 0)
   *  new Coll[Int]()
   *  res0: Coll[Int] = ...
   *
   *  scala> val x = new Coll[String]()
   *  x: Coll[String] = ...
   *
   *  scala> x.filter(_ != "")
   *  \$line11.\$read.\$iw.\$iw.\$iw.\$iw.\$iw.\$iw.\$iw.\$iw.\$iw.\$iw.\$iw.\$iw.x
   *  res1 @ 35563b4b: x.type = ...
   *  }}}
   *
   *  Note how the value of `prefix` changes depending on the qualifier of the macro call
   *  (i.e. the expression that is at the left-hand side of the dot).
   *
   *  Another noteworthy thing about the snippet above is the `Context { type PrefixType = Coll[T] }`
   *  type that is used to stress that the macro implementation works with prefixes of type `Coll[T]`.
   */
  val prefix: Expr[PrefixType]
}
