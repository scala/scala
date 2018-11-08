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

package scala.tools.nsc

import scala.util.control.NonFatal

/** Registry for resources to close when `Global` is closed */
final class CloseableRegistry {
  private[this] var closeables: List[java.io.Closeable] = Nil
  final def registerClosable(c: java.io.Closeable): Unit = {
    closeables ::= c
  }

  def close(): Unit = {
    for (c <- closeables) {
      try {
        c.close()
      } catch {
        case NonFatal(_) =>
      }
    }
    closeables = Nil
  }
}
