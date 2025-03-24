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
package macros

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *  @see [[scala.reflect.api.Internals]]
 */
trait Internals {
  self: blackbox.Context =>

  /** @see [[scala.reflect.api.Internals]] */
  val internal: ContextInternalApi

  /** @see [[scala.reflect.api.Internals]] */
  trait ContextInternalApi extends universe.MacroInternalApi {
    /** Symbol associated with the innermost enclosing lexical context.
     *  Walking the owner chain of this symbol will reveal information about more and more enclosing contexts.
     */
    def enclosingOwner: Symbol

    /** Functions that are available during [[transform]].
     *  @see [[transform]]
     */
    trait TransformApi {
      /** Calls the current transformer on the given tree.
       *  Current transformer = argument to the `transform` call.
       */
      def recur(tree: Tree): Tree

      /** Calls the default transformer on the given tree.
       *  Default transformer = recur into tree's children and assemble the results.
       */
      def default(tree: Tree): Tree
    }

    /** Transforms a given tree using the provided function.
     *  @see [[TransformApi]]
     */
    // TODO: explore a more concise notation that Denys and I discussed today
    // when transformer is PartialFunction[Tree, Tree]] and TransformApi is passed magically
    // also cf. https://github.com/dsl-paradise/dsl-paradise
    def transform(tree: Tree)(transformer: (Tree, TransformApi) => Tree): Tree

    /** Functions that are available during [[typingTransform]].
     *  @see [[typingTransform]]
     */
    trait TypingTransformApi extends TransformApi {
      /** Temporarily pushes the given symbol onto the owner stack, creating a new local typer,
       *  invoke the given operation and then rollback the changes to the owner stack.
       */
      def atOwner[T](owner: Symbol)(op: => T): T

      /** Temporarily pushes the given tree onto the recursion stack, and then calls `atOwner(symbol)(trans)`.
       */
      def atOwner[T](tree: Tree, owner: Symbol)(op: => T): T

      /** Returns the symbol currently on the top of the owner stack.
       *  If we're not inside any `atOwner` call, then macro application's context owner will be used.
       */
      def currentOwner: Symbol

      /** Typechecks the given tree using the local typer currently on the top of the owner stack.
       *  If we're not inside any `atOwner` call, then macro application's callsite typer will be used.
       */
      def typecheck(tree: Tree): Tree
    }

    /** Transforms a given tree using the provided function.
     *  @see [[TypingTransformApi]]
     */
    def typingTransform(tree: Tree)(transformer: (Tree, TypingTransformApi) => Tree): Tree

    /** Transforms a given tree at a given owner using the provided function.
     *  @see [[TypingTransformApi]]
     */
    def typingTransform(tree: Tree, owner: Symbol)(transformer: (Tree, TypingTransformApi) => Tree): Tree
  }
}
