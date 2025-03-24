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

package scala.reflect.macros
package contexts

trait Internals extends scala.tools.nsc.transform.TypingTransformers {
  self: Context =>

  import global._

  lazy val internal: ContextInternalApi = new global.SymbolTableInternal with ContextInternalApi {
    val enclosingOwner = callsiteTyper.context.owner

    class HofTransformer(hof: (Tree, TransformApi) => Tree) extends AstTransformer {
      val api = new TransformApi {
        def recur(tree: Tree): Tree = hof(tree, this)
        def default(tree: Tree): Tree = superTransform(tree)
      }
      def superTransform(tree: Tree) = super.transform(tree)
      override def transform(tree: Tree): Tree = hof(tree, api)
    }

    def transform(tree: Tree)(transformer: (Tree, TransformApi) => Tree): Tree = new HofTransformer(transformer).transform(tree)

    class HofTypingTransformer(hof: (Tree, TypingTransformApi) => Tree) extends TypingTransformer(callsiteTyper.context.unit) { self =>
      currentOwner = callsiteTyper.context.owner
      curTree = EmptyTree
      localTyper = global.analyzer.newTyper(callsiteTyper.context.make(unit = callsiteTyper.context.unit))

      val api = new TypingTransformApi {
        def recur(tree: Tree): Tree = hof(tree, this)
        def default(tree: Tree): Tree = superTransform(tree)
        def atOwner[T](owner: Symbol)(op: => T): T = self.atOwner(owner)(op)
        def atOwner[T](tree: Tree, owner: Symbol)(op: => T): T = self.atOwner(tree, owner)(op)
        def currentOwner: Symbol = self.currentOwner
        def typecheck(tree: Tree): Tree = localTyper.typed(tree)
      }
      def superTransform(tree: Tree) = super.transform(tree)
      override def transform(tree: Tree): Tree = hof(tree, api)
    }

    def typingTransform(tree: Tree)(transformer: (Tree, TypingTransformApi) => Tree): Tree = new HofTypingTransformer(transformer).transform(tree)

    def typingTransform(tree: Tree, owner: Symbol)(transformer: (Tree, TypingTransformApi) => Tree): Tree = {
      val trans = new HofTypingTransformer(transformer)
      trans.atOwner(owner)(trans.transform(tree))
    }
    override def markForAsyncTransform(owner: Symbol, method: DefDef, awaitSymbol: Symbol, config: Map[String, AnyRef]): DefDef = {
      global.async.markForAsyncTransform(owner, method, awaitSymbol, config)
    }
  }
}
