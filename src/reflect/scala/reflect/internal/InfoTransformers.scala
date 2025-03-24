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
package internal

import scala.annotation.tailrec

trait InfoTransformers {
  self: SymbolTable =>

  /* Syncnote: This should not need to be protected, as reflection does not run in multiple phases.
   */
  abstract class InfoTransformer {
    var prev: InfoTransformer = this
    var next: InfoTransformer = this

    val pid: Phase#Id
    val changesBaseClasses: Boolean
    def transform(sym: Symbol, tpe: Type): Type

    @tailrec
    final def insert(that: InfoTransformer): Unit = {
      assert(this.pid != that.pid, this.pid)

      if (that.pid < this.pid) {
        prev insert that
      } else if (next.pid <= that.pid && next.pid != NoPhase.id) {
        next insert that
      } else {
        log(s"Inserting info transformer ${phaseOf(that.pid)} following ${phaseOf(this.pid)}")
        that.next = next
        that.prev = this
        next.prev = that
        this.next = that
      }
    }

    /** The InfoTransformer whose (pid == from).
     *  If no such exists, the InfoTransformer with the next
     *  higher pid.
     */
    @tailrec
    final def nextFrom(from: Phase#Id): InfoTransformer =
      if (from == this.pid) this
      else if (from < this.pid)
        if (prev.pid < from) this
        else prev.nextFrom(from)
      else if (next.pid == NoPhase.id) next
      else next.nextFrom(from)
  }
}

