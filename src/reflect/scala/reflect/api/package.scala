package scala.reflect

import scala.reflect.api.{Universe => ApiUniverse}

/**
 * The main package of Scala's reflection library.
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
 * are defined in packages [[scala.reflect.api]] and `scala.tools.reflect`.
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
 */
package object api {

  // anchors for materialization macros emitted during tag materialization in Implicits.scala
  // implementation is hardwired into `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  // todo. once we have implicit macros for tag generation, we can remove these anchors
  private[scala] def materializeWeakTypeTag[T](u: ApiUniverse): u.WeakTypeTag[T] = ??? // macro
  private[scala] def materializeTypeTag[T](u: ApiUniverse): u.TypeTag[T] = ??? // macro
}