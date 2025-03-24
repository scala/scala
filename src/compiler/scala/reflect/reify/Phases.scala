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

package scala.reflect.reify

import phases._

trait Phases extends Reshape
                with Calculate
                with Metalevels
                with Reify {

  self: Reifier =>

  import global._

  private var alreadyRun = false

  lazy val mkReificationPipeline: Tree => Tree = tree0 => {
    assert(!alreadyRun, "reifier instance cannot be used more than once")
    alreadyRun = true

    var tree = tree0

    if (reifyDebug) println("[calculate phase]")
    calculate.traverse(tree)

    if (reifyDebug) println("[reshape phase]")
    tree = reshape.transform(tree)
    if (reifyDebug) println("[interlude]")
    if (reifyDebug) println("reifee = " + (if (settings.Xshowtrees.value || settings.XshowtreesCompact.value || settings.XshowtreesStringified.value) "\n" + nodePrinters.nodeToString(tree).trim else tree.toString))

    if (reifyDebug) println("[calculate phase]")
    calculate.traverse(tree)

    if (reifyDebug) println("[metalevels phase]")
    tree = metalevels.transform(tree)
    if (reifyDebug) println("[interlude]")
    if (reifyDebug) println(symtab.debugString)

    if (reifyDebug) println("[reify phase]")
    val result = reify(tree)

    result
  }
}
