/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.transform.async

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Flags

trait AsyncAnalysis extends TransformUtils  {
  import u._

  /**
   * Analyze the contents of an `async` block in order to:
   * - Report unsupported `await` calls under nested templates, functions, by-name arguments.
   *
   * Must be called on the original tree, not on the ANF transformed tree.
   */
  def reportUnsupportedAwaits(tree: Tree): Unit = {
    val analyzer = new UnsupportedAwaitAnalyzer
    analyzer.traverse(tree)
    // analyzer.hasUnsupportedAwaits // XB: not used?!
  }

  private class UnsupportedAwaitAnalyzer extends AsyncTraverser {
    var hasUnsupportedAwaits = false

    override def nestedClass(classDef: ClassDef): Unit = {
      val kind = if (classDef.symbol.asClass.isTrait) "trait" else "class"
      reportUnsupportedAwait(classDef, s"nested $kind")
    }

    override def nestedModule(module: ModuleDef): Unit = {
      reportUnsupportedAwait(module, "nested object")
    }

    override def nestedMethod(defDef: DefDef): Unit = {
      reportUnsupportedAwait(defDef, "nested method")
    }

    override def byNameArgument(arg: Tree): Unit = {
      reportUnsupportedAwait(arg, "by-name argument")
    }

    override def function(function: Function): Unit = {
      reportUnsupportedAwait(function, "nested function")
    }

    override def patMatFunction(tree: Match): Unit = {
      reportUnsupportedAwait(tree, "nested function")
    }

    override def traverse(tree: Tree): Unit = {
      tree match {
        case Try(_, _, _) if containsAwait(tree)              =>
          reportUnsupportedAwait(tree, "try/catch")
          super.traverse(tree)
        case Return(_)                                        =>
          abort(tree.pos, "return is illegal within a async block")
        case DefDef(mods, _, _, _, _, _) if mods.hasFlag(Flags.LAZY) && containsAwait(tree) =>
          reportUnsupportedAwait(tree, "lazy val initializer")
        case ValDef(mods, _, _, _) if mods.hasFlag(Flags.LAZY) && containsAwait(tree) =>
          reportUnsupportedAwait(tree, "lazy val initializer")
        case CaseDef(_, guard, _) if guard exists isAwait     =>
          // TODO lift this restriction
          reportUnsupportedAwait(tree, "pattern guard")
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
          if (!isAsync(tree))
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
          reportError(tree.pos, s"await must not be used under a $whyUnsupported.")
      }
      badAwaits.nonEmpty
    }

    private def reportError(pos: Position, msg: String): Unit = {
      hasUnsupportedAwaits = true
      abort(pos, msg)
    }
  }
}
