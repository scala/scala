package scala.reflect
package reify

import scala.reflect.reify.phases._

trait Phases extends Calculate
                with Reshape
                with Metalevels
                with Reify {

  self: Reifier =>

  import mirror._
  import definitions._

  private var alreadyRun = false

  lazy val mkReificationPipeline: Tree => Tree = tree0 => {
    assert(!alreadyRun, "reifier instance cannot be used more than once")
    alreadyRun = true

    var tree = tree0

    if (reifyDebug) println("[calculate phase]")
    calculate.traverse(tree)

    if (reifyDebug) println("[reshape phase]")
    tree = reshape.transform(tree)

    if (reifyDebug) println("[metalevels phase]")
    tree = metalevels.transform(tree)

    if (reifyDebug) println("[interlude]")
    if (reifyDebug) println("symbol table = " + (if (symbolTable.length == 0) "<empty>" else ""))
    if (reifyDebug) symbolTable foreach (println(_))
    if (reifyDebug) println("reifee = " + (if (opt.showTrees) "\n" + nodePrinters.nodeToString(tree).trim else tree.toString))
    if (reifyDebug) println("[reify phase]")
    var result = reify(tree)

    result
  }
}