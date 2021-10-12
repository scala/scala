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

package scala.collection.mutable

import org.scalacheck._
import org.scalacheck.Prop._

object ArrayBufferProperties extends Properties("mutable.ArrayBuffer") {

  type Elem = Int

  property("view consistency after modifications") = forAll { (buf: ArrayBuffer[Elem]) =>
    def check[U](op: ArrayBuffer[Elem] => U): Prop = {
      val b    = buf.clone()
      val view = b.view
      op(b) // modifies the buffer
      b.sameElements(view)
    }

    val spaceForMoreElems = buf.sizeIs <= (Int.MaxValue / 2 - 101)

    (check(_.clear()) :| "_.clear()") &&
      (check(_.dropRightInPlace(1)) :| "_.dropRightInPlace(1)") &&
      (check(_.dropInPlace(1)) :| "_.dropInPlace(1)") &&
      (spaceForMoreElems ==> (check(_ ++= (1 to 100)) :| "_ ++= (1 to 100)")) &&
      (spaceForMoreElems ==> (check(_.prependAll(1 to 100)) :| "_.prependAll(1 to 100)")) &&
      ((!buf.isEmpty && spaceForMoreElems) ==> (check(_.insertAll(1, 1 to 100)) :| "_.insertAll(1, 1 to 100)"))
  }
}
