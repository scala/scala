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
package util

import scala.collection.immutable.Seq

object StringUtil {
  def oxford(vs: Seq[String], conj: String): String =
    vs match {
      case Seq()     => ""
      case Seq(a)    => a
      case Seq(a, b) => s"$a $conj $b"
      case xs        => xs.init.mkString("", ", ", s", $conj ${xs.last}")
    }
}
