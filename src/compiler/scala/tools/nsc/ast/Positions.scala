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

package scala.tools.nsc
package ast

import scala.tools.nsc.Reporting.WarningCategory

trait Positions extends scala.reflect.internal.Positions {
  self: Global =>

  class ValidatingPosAssigner extends PosAssigner {
    var pos: Position = _
    override def traverse(t: Tree): Unit = {
      if (t eq EmptyTree) ()
      else if (t.pos == NoPosition) super.traverse(t setPos pos)
      else if (globalPhase.id <= currentRun.picklerPhase.id) {
        // When we prune due to encountering a position, traverse the
        // pruned children so we can warn about those lacking positions.
        t.children foreach { c =>
          if (!c.canHaveAttrs) ()
          else if (c.pos == NoPosition) {
            runReporting.warning(t.pos, " Positioned tree has unpositioned child in phase " + globalPhase, WarningCategory.OtherDebug, currentOwner)
            inform("parent: " + treeSymStatus(t))
            inform(" child: " + treeSymStatus(c) + "\n")
          }
        }
      }
    }
  }

  override protected[this] lazy val posAssigner: PosAssigner =
    if (settings.Yrangepos.value && settings.isDebug || settings.Yposdebug.value) new ValidatingPosAssigner
    else new DefaultPosAssigner
}
