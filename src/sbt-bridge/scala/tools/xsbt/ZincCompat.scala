/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import java.nio.file.Path

import scala.reflect.io.AbstractFile

object ZincCompat {
  type PlainNioFile = scala.reflect.io.PlainNioFile

  def plainNioFile(path: Path): AbstractFile = new PlainNioFile(path)
  def unwrapPlainNioFile(pf: PlainNioFile): Path = pf.nioPath
}
