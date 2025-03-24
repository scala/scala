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

import xsbti.{ PathBasedFile, VirtualFile }

private trait AbstractZincFile extends scala.reflect.io.AbstractFile {
  def underlying: VirtualFile
}

private final class ZincPlainFile private[xsbt] (val underlying: PathBasedFile)
    extends scala.reflect.io.PlainFile(scala.reflect.io.Path(underlying.toPath.toFile))
    with AbstractZincFile

private final class ZincVirtualFile private[xsbt] (val underlying: VirtualFile)
    extends scala.reflect.io.VirtualFile(underlying.name, underlying.id)
    with AbstractZincFile {
  val buffer = new Array[Byte](4096)

  val in = underlying.input()
  val output0 = output

  try {
    var readBytes = in.read(buffer)

    while (readBytes != -1) {
      output0.write(buffer, 0, readBytes)
      readBytes = in.read(buffer)
    }
  } finally {
    in.close()
    output0.close()
  }
}

private object AbstractZincFile {
  def apply(virtualFile: VirtualFile): AbstractZincFile = virtualFile match {
    case file: PathBasedFile => new ZincPlainFile(file)
    case _                   => new ZincVirtualFile(virtualFile)
  }

  def unapply(file: scala.reflect.io.AbstractFile): Option[VirtualFile] = file match {
    case wrapper: AbstractZincFile => Some(wrapper.underlying)
    case _                         => None
  }
}
