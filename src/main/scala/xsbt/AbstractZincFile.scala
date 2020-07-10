/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

import xsbti.{ PathBasedFile, VirtualFile }
import scala.reflect.io.Streamable

private trait AbstractZincFile extends scala.reflect.io.AbstractFile {
  def underlying: VirtualFile
}

private final class ZincPlainFile private[xsbt] (val underlying: PathBasedFile)
    extends scala.reflect.io.PlainFile(scala.reflect.io.Path(underlying.toPath.toFile))
    with AbstractZincFile

private final class ZincVirtualFile private[xsbt] (val underlying: VirtualFile)
    extends scala.reflect.io.VirtualFile(underlying.name, underlying.id)
    with AbstractZincFile {
  Streamable.closing(output)(_.write(Streamable.bytes(underlying.input))) // fill in the content
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
