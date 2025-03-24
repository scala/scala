/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * `Universe` provides a complete set of reflection operations which make it possible for one
 * to reflectively inspect Scala type relations, such as membership or subtyping.
 *
 * [[scala.reflect.api.Universe]] has two specialized sub-universes for different scenarios.
 * [[scala.reflect.api.JavaUniverse]] adds operations that link symbols and types to the underlying
 * classes and runtime values of a JVM instance-- this can be thought of as the `Universe` that
 * should be used for all typical use-cases of Scala reflection. [[scala.reflect.macros.Universe]]
 * adds operations which allow macros to access selected compiler data structures and operations--
 * this type of `Universe` should only ever exist within the implementation of a Scala macro.
 *
 * `Universe` can be thought of as the entry point to Scala reflection. It mixes-in, and thus provides
 * an interface to the following main types:
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
 * To obtain a `Universe` to use with Scala runtime reflection, simply make sure to use or import
 * `scala.reflect.runtime.universe._`
 *   {{{
 *   scala> import scala.reflect.runtime.universe._
 *   import scala.reflect.runtime.universe._
 *
 *   scala> typeOf[List[Int]]
 *   res0: reflect.runtime.universe.Type = scala.List[Int]
 *
 *   scala> typeOf[Either[String, Int]]
 *   res1: reflect.runtime.universe.Type = scala.Either[String,Int]
 *   }}}
 *
 * To obtain a `Universe` for use within a Scala macro, use [[scala.reflect.macros.blackbox.Context#universe]].
 * or [[scala.reflect.macros.whitebox.Context#universe]]. For example:
 * {{{
 *  def printf(format: String, params: Any*): Unit = macro impl
 *  def impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
 *    import c.universe._
 *    ...
 *  }
 * }}}
 *
 * For more information about `Universe`s, see the [[https://docs.scala-lang.org/overviews/reflection/environment-universes-mirrors.html Reflection Guide: Universes]]
 *
 *  @groupprio Universe -1
 *  @group ReflectionAPI
 *
 *  @contentDiagram hideNodes "*Api"
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
                           with ImplicitTags
                           with StandardDefinitions
                           with StandardNames
                           with StandardLiftables
                           with Mirrors
                           with Printers
                           with Liftables
                           with Quasiquotes
                           with Internals
{
  /** Use `reify` to produce the abstract syntax tree representing a given Scala expression.
   *
   * For example:
   *
   * {{{
   * val five = reify{ 5 }         // Literal(Constant(5))
   * reify{ 5.toString }           // Apply(Select(Literal(Constant(5)), TermName("toString")), List())
   * reify{ five.splice.toString } // Apply(Select(five, TermName("toString")), List())
   * }}}
   *
   * The produced tree is path dependent on the Universe `reify` was called from.
   *
   * Use [[scala.reflect.api.Exprs#Expr.splice]] to embed an existing expression into a `reify` call. Use [[Expr]] to turn a [[Tree]] into an expression that can be spliced.
   * @group Universe
   */
  // implementation is hardwired to `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  def reify[T](expr: T): Expr[T] = macro ???
}
