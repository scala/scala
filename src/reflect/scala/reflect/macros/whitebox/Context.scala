package scala
package reflect
package macros
package whitebox

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
 *  If a macro def refers to a macro impl that uses `whitebox.Context`, then this macro def becomes a whitebox macro,
 *  gaining the ability to refine the type of its expansion beyond its official return type, which enables a number of important use cases.
 *  Blackbox macros, i.e. the ones defined with `blackbox.Context`, can't do that, so they are less powerful.
 *  However blackbox macros are also going to enjoy better support than whitebox macros, so choose wisely.
 *  See the [[http://docs.scala-lang.org/overviews/macros/overview.html Macros Guide]] for more information.
 *
 *  @see `scala.reflect.macros.blackbox.Context`
 */
trait Context extends blackbox.Context {
  /** @inheritdoc
   */
  def openMacros: List[Context]

  /** @inheritdoc
   */
  def enclosingMacros: List[Context]

  /** Information about one of the currently considered implicit candidates.
   *  Candidates are used in plural form, because implicit parameters may themselves have implicit parameters,
   *  hence implicit searches can recursively trigger other implicit searches.
   *
   *  `pre` and `sym` provide information about the candidate itself.
   *  `pt` and `tree` store the parameters of the implicit search the candidate is participating in.
   */
  case class ImplicitCandidate(pre: Type, sym: Symbol, pt: Type, tree: Tree)

  /** Information about one of the currently considered implicit candidates.
   *  Candidates are used in plural form, because implicit parameters may themselves have implicit parameters,
   *  hence implicit searches can recursively trigger other implicit searches.
   *
   *  Can be useful to get information about an application with an implicit parameter that is materialized during current macro expansion.
   *  If we're in an implicit macro being expanded, it's included in this list.
   *
   *  Unlike `enclosingImplicits`, this is a def, which means that it gets recalculated on every invocation,
   *  so it might change depending on what is going on during macro expansion.
   */
  def openImplicits: List[ImplicitCandidate]

  /** Information about one of the currently considered implicit candidates.
   *  Candidates are used in plural form, because implicit parameters may themselves have implicit parameters,
   *  hence implicit searches can recursively trigger other implicit searches.
   *
   *  Can be useful to get information about an application with an implicit parameter that is materialized during current macro expansion.
   *  If we're in an implicit macro being expanded, it's included in this list.
   *
   *  Unlike `openImplicits`, this is a val, which means that it gets initialized when the context is created
   *  and always stays the same regardless of whatever happens during macro expansion.
   */
  def enclosingImplicits: List[ImplicitCandidate]
}