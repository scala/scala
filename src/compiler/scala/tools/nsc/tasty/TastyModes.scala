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

package scala.tools.nsc.tasty

import scala.collection.mutable

/**A static type representing a bitset of modes that affect the interpretation of a TASTy file,
 * such as distinguishing between reading the parents of a class, or an annotation tree.
 */
object TastyModes {

  final val EmptyTastyMode: TastyMode = TastyMode(0)
  /** When reading the parents of a class template */
  final val ReadParents: TastyMode = TastyMode(1 << 0)
  /** When reading trees of an annotation */
  final val ReadAnnotation: TastyMode = TastyMode(1 << 1)
  /** When reading the outermost tree of an term */
  final val OuterTerm: TastyMode = TastyMode(1 << 2)
  /** When reading statements in a sequence */
  final val IndexStats: TastyMode = TastyMode(1 << 3)
  /** When reading a macro definition body */
  final val ReadMacro: TastyMode = TastyMode(1 << 4)
  /** When not at the package scope */
  final val InnerScope: TastyMode = TastyMode(1 << 5)
  /** When reading the tree of an Opaque type */
  final val OpaqueTypeDef: TastyMode = TastyMode(1 << 6)
  /** When reading trees of an annotation */
  final val ReadAnnotationCtor: TastyMode = TastyMode(1 << 7)
  /** When reading a TASTy file produced from a Java source file (file has JAVAattr attribute) */
  final val ReadJava: TastyMode = TastyMode(1 << 8)

  /** The union of `IndexStats` and `InnerScope` */
  final val IndexScopedStats: TastyMode = IndexStats | InnerScope

  final val ReadAnnotTopLevel: TastyMode = ReadAnnotation | ReadAnnotationCtor

  case class TastyMode(val toInt: Int) extends AnyVal { mode =>

    def |(other: TastyMode): TastyMode    = TastyMode(toInt | other.toInt)
    def &(mask: TastyMode): TastyMode     = TastyMode(toInt & mask.toInt)
    def &~(mask: TastyMode): TastyMode    = TastyMode(toInt & ~mask.toInt)
    def is(mask: TastyMode): Boolean      = (this & mask) == mask
    def isOneOf(mask: TastyMode): Boolean = (this & mask).nonEmpty
    def nonEmpty: Boolean                 = toInt != 0

    def debug: String = {
      if (mode == EmptyTastyMode) "EmptyTastyMode"
      else {
        val sb = mutable.ArrayBuffer.empty[String]
        if (mode.is(ReadParents))    sb += "ReadParents"
        if (mode.is(ReadAnnotation)) sb += "ReadAnnotation"
        if (mode.is(OuterTerm))      sb += "OuterTerm"
        if (mode.is(IndexStats))     sb += "IndexStats"
        if (mode.is(ReadMacro))      sb += "ReadMacro"
        if (mode.is(InnerScope))     sb += "InnerScope"
        if (mode.is(OpaqueTypeDef))  sb += "OpaqueTypeDef"
        if (mode.is(ReadAnnotationCtor)) sb += "ReadAnnotationCtor"
        if (mode.is(ReadJava)) sb += "ReadJava"
        sb.mkString(" | ")
      }
    }

  }

}
