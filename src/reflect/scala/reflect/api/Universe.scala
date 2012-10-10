package scala.reflect
package api

/**
 * The Scala reflection cake.
 *
 * See [[scala.reflect.api.package the overview page]] for a description of universes and infomation on getting started with Scala reflection API.
 * This page lists the most important layers of the cake, and describes paculiarities of cake APIs.
 *
 * The reflection library is structured according to the 'cake pattern'. The main layer
 * resides in package [[scala.reflect.api]] and defines an interface to the following main types:
 *
 *   - [[scala.reflect.api.Types#Type Types]] represent types
 *   - [[scala.reflect.api.Symbols#Symbol Symbols]] represent definitions
 *   - [[scala.reflect.api.Trees#Tree Trees]] represent abstract syntax trees
 *   - [[scala.reflect.api.Names#Name Names]] represent term and type names
 *   - [[scala.reflect.api.Annotations#Annotation Annotations]] represent annotations
 *   - [[scala.reflect.api.Positions#Position Positions]] represent source positions of tree nodes
 *   - [[scala.reflect.api.FlagSets#FlagSet FlagSet]] represent sets of flags that apply to symbols and
 *     definition trees
 *   - [[scala.reflect.api.Constants#Constant Constants]] represent compile-time constants.
 *
 * Each of these types are defined in their own enclosing traits, which are ultimately all inherited by class
 * [[scala.reflect.api.Universe Universe]]. The main universe defines a minimal interface to the above types.
 * Universes that provide additional functionality such as deeper introspection or runtime code generation,
 * are defined in packages [[scala.reflect.macros]] and `scala.tools.reflect`.
 *
 * The cake pattern employed here requires to write certain Scala idioms with more indirections that usual.
 * What follows is a description of these indirections, which will help to navigate the Scaladocs easily.
 *
 * For instance, consider the base type of all abstract syntax trees: [[scala.reflect.api.Trees#Tree]].
 * This type is not a class but is abstract and has an upper bound of [[scala.reflect.api.Trees#TreeApi]],
 * which is a class defining the minimal base interface for all trees.
 *
 * For a more interesting tree type, consider [[scala.reflect.api.Trees#If]] representing if-expressions.
 * It is defined next to a value `If` of type [[scala.reflect.api.Trees#IfExtractor]].
 * This value serves as the companion object defining a factory method `apply` and a corresponding `unapply`
 * for pattern matching.
 *
 * {{{
 * import scala.reflect.runtime.universe._
 * val cond = reify{ condition }.tree // <- just some tree representing a condition
 * val body = Literal(Constant(1))
 * val other = Literal(Constant(2))
 * val iftree = If(cond,body,other)
 * }}}
 *
 * is equivalent to
 *
 * {{{
 * import scala.reflect.runtime.universe._
 * val iftree = reify{ if( condition ) 1 else 2 }.tree
 * }}}
 *
 * and can be pattern matched as
 *
 * {{{
 * iftree match { case If(cond,body,other) => ... }
 * }}}
 *
 * Moreover, there is an implicit value [[scala.reflect.api.Trees#IfTag]] of type
 * `ClassTag[If]` that is used by the Scala compiler so that we can indeed pattern match on `If`:
 * {{{
 *   iftree match { case _:If => ... }
 * }}}
 * Without the given implicit value, this pattern match would raise an "unchecked" warning at compile time
 * since `If` is an abstract type that gets erased at runtime. See [[scala.reflect.ClassTag]] for details.
 *
 * To summarize: each tree type `X` (and similarly for other types such as `Type` or `Symbol`) is represented
 * by an abstract type `X`, optionally together with a class `XApi` that defines `X`'s' interface.
 * `X`'s companion object, if it exists, is represented by a value `X` that is of type `XExtractor`.
 * Moreover, for each type `X`, there is a value `XTag` of type `ClassTag[X]` that allows to pattern match on `X`.
 * @groupprio Universe -1
 */
abstract class Universe extends Symbols
                           with Types
                           with FlagSets
                           with Scopes
                           with Names
                           with Trees
                           with Constants
                           with Annotations
                           with Positions
                           with Exprs
                           with TypeTags
                           with TagInterop
                           with StandardDefinitions
                           with StandardNames
                           with BuildUtils
                           with Mirrors
                           with Printers
                           with Importers
{
  /** Produce the abstract syntax tree representing the given Scala expression.
   *
   * For example
   *
   * {{{
   * val five = reify{ 5 }    // Literal(Constant(5))
   * reify{ 2 + 4 }           // Apply( Select( Literal(Constant(2)), newTermName("\$plus")), List( Literal(Constant(4)) ) )
   * reify{ five.splice + 4 } // Apply( Select( Literal(Constant(5)), newTermName("\$plus")), List( Literal(Constant(4)) ) )
   * }}}
   *
   * The produced tree is path dependent on the Universe `reify` was called from.
   *
   * Use [[scala.reflect.api.Exprs#Expr.splice]] to embed an existing expression into a reify call. Use [[Expr]] to turn a [[Tree]] into an expression that can be spliced.
   *
   * == Further info and implementation details ==
   *
   * `reify` is implemented as a macro, which given an expression, generates a tree that when compiled and executed produces the original tree.
   *
   *  For instance in `reify{ x + 1 }` the macro `reify` receives the abstract syntax tree of `x + 1` as its argument, which is
   *
   *  {{{
   *    Apply(Select(Ident("x"), "+"), List(Literal(Constant(1))))
   *  }}}
   *
   *  and returns a tree, which produces the tree above, when compiled and executed. So in other terms, the refiy call expands to something like
   *
   *  {{{
   *      val $u: u.type = u // where u is a reference to the Universe that calls the reify
   *      $u.Expr[Int]($u.Apply($u.Select($u.Ident($u.newFreeVar("x", <Int>, x), "+"), List($u.Literal($u.Constant(1))))))
   *  }}}
   *
   *  ------
   *
   *  Reification performs expression splicing (when processing Expr.splice)
   *  and type splicing (for every type T that has a TypeTag[T] implicit in scope):
   *
   *  {{{
   *    val two = mirror.reify(2)                         // Literal(Constant(2))
   *    val four = mirror.reify(two.splice + two.splice)  // Apply(Select(two.tree, newTermName("\$plus")), List(two.tree))
   *
   *    def macroImpl[T](c: Context) = {
   *      ...
   *      // T here is just a type parameter, so the tree produced by reify won't be of much use in a macro expansion
   *      // however, if T were annotated with c.WeakTypeTag (which would declare an implicit parameter for macroImpl)
   *      // then reification would substitute T with the TypeTree that was used in a TypeApply of this particular macro invocation
   *      val factory = c.reify{ new Queryable[T] }
   *      ...
   *    }
   *  }}}
   *
   *  The transformation looks mostly straightforward, but it has its tricky parts:
   *    - Reifier retains symbols and types defined outside the reified tree, however
   *      locally defined entities get erased and replaced with their original trees
   *    - Free variables are detected and wrapped in symbols of the type `FreeTermSymbol` or `FreeTypeSymbol`
   *    - Mutable variables that are accessed from a local function are wrapped in refs
   * @group Universe
   */
  // implementation is hardwired to `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  def reify[T](expr: T): Expr[T] = ??? // macro
}