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

import java.io.{ InputStream, OutputStream }
import xsbti.{ PathBasedFile, VirtualFile }
import scala.reflect.io.{ AbstractFile, Path, PlainFile }

private trait VirtualFileWrap extends AbstractFile {
  def underlying: VirtualFile
}

private final class XsbtPlainFile(val underlying: PathBasedFile)
    extends PlainFile(Path(underlying.toPath.toFile))
    with VirtualFileWrap

private final class XsbtVirtualFile private[xsbt] (val underlying: VirtualFile)
    extends reflect.io.VirtualFile(underlying.name, underlying.id)
    with VirtualFileWrap {

  // fill the in-memory reflect.io.VirtualFile with the content of the underlying xsbti.VirtualFile
  copyTo(underlying.input(), output)

  private def copyTo(input: InputStream, output: OutputStream): Unit = {
    while (input.available > 0) {
      val content = new Array[Byte](input.available)
      input.read(content)
      output.write(content)
    }
    input.close()
    output.close()
  }
}

private object VirtualFileWrap {
  def apply(virtualFile: VirtualFile): VirtualFileWrap = virtualFile match {
    case file: PathBasedFile => new XsbtPlainFile(file)
    case _                   => new XsbtVirtualFile(virtualFile)
  }

  def unapply(abstractFile: AbstractFile): Option[VirtualFile] = abstractFile match {
    case wrapper: VirtualFileWrap => Some(wrapper.underlying)
    case _                        => None
  }
}
