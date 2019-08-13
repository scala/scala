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

package scala
package tools.nsc
package interpreter

private[interpreter] object Util {
  def lastOf[A](it: Iterator[A]): A = {
    var res: A = it.next()
    while (it.hasNext)
      res = it.next()
    res
  }
}