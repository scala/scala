/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt

import java.nio.file.Path

import scala.reflect.io.AbstractFile

object ZincCompat {
  type PlainNioFile = scala.reflect.io.PlainNioFile

  def plainNioFile(path: Path): AbstractFile = new PlainNioFile(path)
  def unwrapPlainNioFile(pf: PlainNioFile): Path = pf.nioPath
}
