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

import xsbti.VirtualFile
import scala.reflect.io.AbstractFile
import java.io.{ File, InputStream, OutputStream }

final class VirtualFileWrap(val underlying: VirtualFile) extends AbstractFile {
  // scala.tools.nsc.CompilationUnits$CompilationUnit.<init>(CompilationUnits.scala:161)
  override def name: String = underlying.name

  // scala.tools.nsc.Global$Run.addUnit(Global.scala:1353)
  override def path: String = underlying.id

  // at scala.tools.nsc.io.SourceReader.read(SourceReader.scala:62)
  override def input: InputStream = underlying.input

  override def absolute: AbstractFile = {
    ???
    // abstractFile.absolute
  }

  // used only by Scala 2.10
  // https://github.com/scala/scala/blob/v2.10.7/src/compiler/scala/tools/nsc/Global.scala#L1726
  override def container: AbstractFile = {
    new AbstractFile {
      override def name: String = "temp"
      def absolute: AbstractFile = ???
      def container: AbstractFile = ???
      def create(): Unit = ???
      def delete(): Unit = ???
      def file: File = ???
      def input: InputStream = ???
      def isDirectory: Boolean = true
      def iterator: Iterator[AbstractFile] = ???
      def lastModified: Long = ???
      def lookupName(name: String, directory: Boolean): AbstractFile = ???
      def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = ???
      def output: OutputStream = ???
      def path: String = ???
    }
  }

  override def file: File = {
    null
  }

  override def create(): Unit = {
    ???
    // abstractFile.create()
  }
  override def delete(): Unit = {
    ???
    /// abstractFile.delete()
  }
  override def isDirectory: Boolean = {
    ???
    // abstractFile.isDirectory
  }
  override def lastModified: Long = {
    ???
    // abstractFile.lastModified
  }

  override def output: OutputStream = {
    ???
    // abstractFile.output
  }
  override def iterator: Iterator[AbstractFile] = {
    ???
    // abstractFile.iterator
  }
  override def lookupName(name: String, directory: Boolean): AbstractFile = {
    ???
    // abstractFile.lookupName(name, directory)
  }
  override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = {
    ???
    // abstractFile.lookupNameUnchecked(name, directory)
  }
}
