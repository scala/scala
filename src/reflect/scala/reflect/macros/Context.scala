package scala.reflect
package macros

// todo. introduce context hierarchy
// the most lightweight context should just expose the stuff from the SIP
// the full context should include all traits from scala.reflect.macros (and probably reside in scala-compiler.jar)

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  The Scala macros context.
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
                 with Synthetics {

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
   *  For a example, for a macro `filter` defined as an instance method on a collection `Coll`,
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
