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

package scala.tools.nsc.tasty

import TastyUnpickler.SectionUnpickler

import scala.tools.tasty.{TastyRefs, TastyReader, TastyName}, TastyRefs.NameRef

import scala.reflect.io.AbstractFile

object ScalacUnpickler {

  final class TreeSectionUnpickler[Tasty <: TastyUniverse](implicit tasty: Tasty)
  extends SectionUnpickler[TreeUnpickler[Tasty]]("ASTs") { self =>
    def unpickle(reader: TastyReader, nameAtRef: NameRef => TastyName): TreeUnpickler[Tasty] =
      new TreeUnpickler(reader, nameAtRef, Seq.empty)
  }

  object Unpickler {
    def tasty[Tasty <: TastyUniverse](implicit tasty: Tasty) = tasty
  }

  final implicit class Unpickler[Tasty <: TastyUniverse](private val tasty: Tasty) extends AnyVal {
    import tasty._

    /** Unpickle symbol table information descending from a class and/or module root
     *  from an array of bytes.
     *  @param classRoot  the top-level class which is unpickled
     *  @param moduleRoot the top-level module which is unpickled
     *  @param filename   filename associated with bytearray, only used for error messages
     */
    def unpickle(bytes: Array[Byte]/*, mode: UnpickleMode = UnpickleMode.TopLevel*/, classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String): Unit = {
      implicit val ctx: Context = new InitialContext(classRoot, AbstractFile.getFile(filename))

      ctx.log(s"Unpickling $filename")

      val unpickler = new TastyUnpickler[tasty.type](bytes)(tasty)
      unpickler.readSections()
      val treeUnpickler = unpickler.unpickle[TreeUnpickler[tasty.type]](new TreeSectionUnpickler()(tasty)).get
      treeUnpickler.enter(classRoot, moduleRoot)
    }
  }
}
