package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  The whitebox Scala macros context.
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
 *  If a macro def refers to a macro impl that uses `WhiteboxContext`, then this macro def becomes a whitebox macro,
 *  gaining the ability to refine the type of its expansion beyond its official return type, which enables a number of important use cases.
 *  Blackbox macros, i.e. the ones defined with `BlackboxContext`, can't do that, so they are less powerful.
 *  However blackbox macros are also going to enjoy better support than whitebox macros, so choose wisely.
 *  See the [[http://docs.scala-lang.org/overviews/macros.html Macros Guide]] for more information.
 *
 *  @see `scala.reflect.macros.BlackboxContext`
 */
trait WhiteboxContext extends BlackboxContext {
  /** @inheritdoc
   */
  def openMacros: List[WhiteboxContext]

  /** @inheritdoc
   */
  def enclosingMacros: List[WhiteboxContext]
}