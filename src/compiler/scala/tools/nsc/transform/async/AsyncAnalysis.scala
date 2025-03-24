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

package scala.tools.nsc.transform.async

import scala.collection.mutable.ListBuffer

trait AsyncAnalysis extends TransformUtils  {
  import global._

  /**
   * Analyze the contents of an `async` block in order to:
   * - Report unsupported `await` calls under nested templates, functions, by-name arguments.
   *
   * Must be called on the original tree, not on the ANF transformed tree.
   */
  def reportUnsupportedAwaits(tree: Tree): Unit = {
    val analyzer = new UnsupportedAwaitAnalyzer
    analyzer.traverse(tree)
  }

  private class UnsupportedAwaitAnalyzer extends AsyncTraverser {
    override def nestedClass(classDef: ClassDef): Unit = {
      val kind = if (classDef.symbol.isTrait) "trait" else "class"
      reportUnsupportedAwait(classDef, s"nested $kind")
    }

    override def nestedModuleClass(moduleClass: ClassDef): Unit = {
      reportUnsupportedAwait(moduleClass, "nested object")
    }

    override def nestedMethod(defDef: DefDef): Unit = {
      if (defDef.symbol.isArtifact && defDef.name.startsWith(nme.LIFTED_TREE))
        reportUnsupportedAwait(defDef, "try/catch")
      else
        reportUnsupportedAwait(defDef, "nested method")
    }

    override def synchronizedCall(arg: Tree): Unit = {
      reportUnsupportedAwait(arg, "synchronized call")
    }

    override def function(function: Function): Unit = {
      reportUnsupportedAwait(function, "nested function")
    }
    override def function(expandedFunction: ClassDef): Unit = {
      reportUnsupportedAwait(expandedFunction, "nested function")
    }

    override def traverse(tree: Tree): Unit = {
      tree match {
        case Try(_, _, _) if containsAwait(tree)              =>
          reportUnsupportedAwait(tree, "try/catch")
          super.traverse(tree)
        case Throw(Apply(fun, Ident(name) :: _)) if fun.symbol.isConstructor && fun.symbol.owner == definitions.NonLocalReturnControlClass && name.startsWith(nme.NON_LOCAL_RETURN_KEY_STRING) =>
          global.reporter.error(tree.pos, "return is illegal within a async block")
        case DefDef(mods, _, _, _, _, _) if tree.symbol.name.endsWith(nme.LAZY_SLOW_SUFFIX) && containsAwait(tree) =>
          reportUnsupportedAwait(tree, "lazy val initializer")
        case _                                                =>
          super.traverse(tree)
      }
    }

    /**
     * @return true, if the tree contained an unsupported await.
     */
    private def reportUnsupportedAwait(tree: Tree, whyUnsupported: String): Boolean = {
      val badAwaits = ListBuffer[Tree]()
      object traverser extends Traverser {
        override def traverse(tree: Tree): Unit = {
          super.traverse(tree)
          tree match {
            case rt: RefTree if isAwait(rt) =>
              badAwaits += rt
            case _ =>
          }
        }
      }
      traverser(tree)
      badAwaits foreach {
        tree =>
          reportError(tree.pos, s"${currentTransformState.Async_await.decodedName} must not be used under a $whyUnsupported.")
      }
      badAwaits.nonEmpty
    }

    private def reportError(pos: Position, msg: String): Unit = {
      global.reporter.error(pos, msg)
    }
  }
}
