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
package backend
package jvm

import scala.collection.mutable

abstract class DebugInfoBuilder extends PerRunInit {
  val postProcessor: PostProcessor

  import postProcessor.bTypes
  import bTypes.{LazyVar, perRunLazy}

  import DebugInfoBuilder._
  import DebugInfoWriter._

  private[this] lazy val debugInfoWriters: LazyVar[mutable.Map[String, DebugInfoWriter]] = perRunLazy(this)(mutable.Map.empty)

  def registerCUnitClasses(cUnit: GeneratedCompilationUnit): Unit = {
    for (cls <- cUnit.classes.filter(!_.isArtifact)) { // don't generate debug info for artifacts (e.g. mirror classes)
      debugInfoWriters.get += (cls.classNode.name -> new JSR45DebugInfoWriter(Some(cls)))
    }
  }

  def addInlineEntry(fqCallsiteClass: String, entry: InlineEntry): Option[Int] = {
    debugInfoWriters.get.getOrElse(fqCallsiteClass, noOpDebugInfoWriter).writeInlineEntry(entry)(debugInfoWriters.get)
  }

  def putDebugInfoTo(cUnit: GeneratedCompilationUnit): Unit = {
    for (cls <- cUnit.classes.filter(!_.isArtifact)) { // don't generate debug info for artifacts (e.g. mirror classes)
      val writer = debugInfoWriters.get(cls.classNode.name)
      cls.classNode.visitSource(writer.generatedClass.get.classNode.sourceFile, writer.SMAP().getOrElse(""))
    }
  }

}

object DebugInfoBuilder {
  sealed abstract class InlineEntry
  case class InlineMapping(callsiteLine: Int, inlineLine: Int,
                           calleeFileName: String, calleeInternalName: String) extends InlineEntry
  case object InlineSeparator extends InlineEntry

  /**
   * Case class modelling a raw line mapping.
   *
   * @param from The input line number.
   * @param to The output line number.
   * @param sourceFileId The source file ID for this raw mapping.
   *                     Our mappings always have a sourceFileID, we use an Option here
   *                     to remain compatible with the JSR45 specification.
   */
  case class RawLineMapping(from: Int, to: Int, sourceFileId: Option[Int]) extends Ordered[RawLineMapping]{
    override def compare(that: RawLineMapping): Int =
      implicitly[Ordering[(Option[Int], Int, Int)]].compare((this.sourceFileId, this.from, this.to),
                                                            (that.sourceFileId, that.from, that.to))
  }

  object DebugInfoWriter {
    import JSR45._

    private[DebugInfoBuilder] val noOpDebugInfoWriter: DebugInfoWriter = new NoOpDebugInfoWriter

    /**
     * Models a debug info writer for a specific class.
     *
     * The writer "writes" (i.e. builds) the debug information in memory
     * and can compute the final SMAP via the `computeSMAP()` call.
     */
    sealed trait DebugInfoWriter {
      /**
       * The class node of the class corresponding to this writer.
       */
      val generatedClass: Option[GeneratedClass]

      /**
       * Add a mapping for an inline line from an external source to this class source.
       */
      def writeInlineEntry(entry: InlineEntry)(implicit debugInfoWriters: mutable.Map[String, DebugInfoWriter]): Option[Int]

      /**
       * Return the SMAP string from the collected information.
       *
       * @return the String that should be written to the SourceDebugExtension class attribute.
       */
      def SMAP(): Option[String]
    }

    class JSR45DebugInfoWriter(override val generatedClass: Option[GeneratedClass]) extends DebugInfoWriter {

      private var scalaStratum = {
        val fileSection = generatedClass.fold(Seq.empty[FileSectionEntry]) { cls =>
          Seq(FileSectionEntry(cls.position.source.file.name, Some(cls.position.source.path)))
        }

        val lineSection = generatedClass.fold(Seq.empty[LineSectionEntry]) { cls =>
          Seq(LineSectionEntry(
            inputStartLine = 1,
            lineFileId = Some(1),
            repeatCount = Some(cls.position.source.lineCount),
            outputStartLine = 1
            ))
        }

        JSR45.Stratum(name = "Scala", fileSection, lineSection, generatedClass.fold(0)(_.position.source.lineCount))
      }

      override def writeInlineEntry(lineEntry: InlineEntry)(implicit debugInfoWriters: mutable.Map[String, DebugInfoWriter]): Option[Int] = {
        def ensureFileEntry(stratum: JSR45.Stratum, fileEntry: FileSectionEntry): JSR45.Stratum = {
          stratum.fileSection.find(_ == fileEntry).fold {
            stratum.copy(fileSection = stratum.fileSection :+ fileEntry)
          } (_ => stratum)
        }

        def ensureLineEntry(stratum: JSR45.Stratum, line: Int, fileEntry: FileSectionEntry): JSR45.Stratum = {
          val withFileEntry = ensureFileEntry(stratum, fileEntry)
          val calleeFileId = withFileEntry.fileSection.indexOf(fileEntry) + 1

          val currentLineSection = withFileEntry.lineSection
          val nextLineMapping = withFileEntry.lineCount + 1

          val newLineSection = currentLineSection match {
            case start
              :+ (preLastEntry @ LineSectionEntry(inputStartLinePreLast,
                                                  Some(fileIdPreLast),
                                                  repeatCountPreLast,
                                                  _,
                                                  Some(outputLineIncrementPreLast)))
              :+ LineSectionEntry(inputStartLineLast,
                                  Some(fileIdLast),
                                  _,
                                  outputStartLineLast,
                                  _)
              if fileIdPreLast == fileIdLast && fileIdLast == calleeFileId
                && inputStartLinePreLast + repeatCountPreLast.getOrElse(1) == inputStartLineLast
                && inputStartLineLast == line
                && outputLineIncrementPreLast == nextLineMapping - outputStartLineLast + 1 =>
              start :+ preLastEntry.copy(repeatCount = Some(repeatCountPreLast.getOrElse(1) + 1))

            case start :+ (lastEntry@LineSectionEntry(inputStartLine, Some(fileId), repeatCount, outputStartLine, None))
              if fileId == calleeFileId
                && inputStartLine + repeatCount.getOrElse(1) == line
                && outputStartLine + repeatCount.getOrElse(1) == nextLineMapping =>
              start :+ lastEntry.copy(repeatCount = Some(repeatCount.getOrElse(1) + 1))

            case start :+ (lastEntry @ LineSectionEntry(inputStartLine, Some(fileId), None, outputStartLine, outputLineIncrement))
              if fileId == calleeFileId
                && inputStartLine == line
                && outputStartLine + outputLineIncrement.getOrElse(1) == nextLineMapping =>
              start :+ lastEntry.copy(outputLineIncrement = Some(outputLineIncrement.getOrElse(1) + 1))

            case Nil =>
              Seq(LineSectionEntry(line, Some(calleeFileId), None, nextLineMapping, None))

            case _ =>
              currentLineSection :+ LineSectionEntry(line, Some(calleeFileId), None, nextLineMapping, None)

          }

          withFileEntry.copy(lineSection = newLineSection, lineCount = nextLineMapping)
        }

        lineEntry match {
          case InlineMapping(_, inlineLine, calleeFileName, calleeInternalName) =>
            val (line, fileEntry) = getRecursiveInlineInfo(calleeInternalName, inlineLine) match {
              case Some(info) => info
              case None => (inlineLine, FileSectionEntry(calleeFileName, Some(calleeInternalName)))
            }

            scalaStratum = ensureLineEntry(scalaStratum, line, fileEntry)
            Some(scalaStratum.lineCount)

          case InlineSeparator =>
            None
        }
      }

      override def SMAP(): Option[String] = {
        Some(DebugInfo(generatedClass.fold("")(_.position.source.file.name), Seq(scalaStratum)).toString)
      }

      private def getRecursiveInlineInfo(calleeInternalName: String, line: Int)(implicit debugInfoWriters: mutable.Map[String, DebugInfoWriter]): Option[(Int, FileSectionEntry)] = {
        debugInfoWriters.get(calleeInternalName).flatMap {
          case writer: JSR45DebugInfoWriter =>
            writer.scalaStratum.findReverseLineMapping(line).flatMap {
              case RawLineMapping(from, to, sourceFileId) if from != to =>
                writer.scalaStratum.fileSection(sourceFileId.getOrElse(1) - 1).absoluteFilePathOrInternalName.flatMap {
                  getRecursiveInlineInfo(_, from)
                }
              case RawLineMapping(from, _, sourceFileId) =>
                Some((from, writer.scalaStratum.fileSection(sourceFileId.getOrElse(1) - 1)))
            }

          case _: NoOpDebugInfoWriter => None
        }
      }

    }

    class NoOpDebugInfoWriter extends DebugInfoWriter {
      val generatedClass: Option[GeneratedClass] = None
      override def writeInlineEntry(entry: InlineEntry)(implicit debugInfoWriters: mutable.Map[String, DebugInfoWriter]): Option[Int] = None
      override def SMAP(): Option[String] = None
    }
  }

  object JSR45 {

    /**
     * Case class modelling a JSR45 file section entry.
     *
     * @param fileName The name of the file, with extension (sans path).
     * @param absoluteFilePathOrInternalName The absolute file path (if available), otherwise the internal class name.
     *                                       Examples: `/example/dir/ClassName.scala`, `scala/collection/immutable/Seq`.
     */
    case class FileSectionEntry(fileName: String, absoluteFilePathOrInternalName: Option[String] = None)

    /**
     * Case class modelling a JSR45 line section entry.
     *
     * For example, the following collection of [[LineSectionEntry]] instances:
     * {{{
     * Seq(
     *   LineSectionEntry(123, None, None, 207, None)
     *   LineSectionEntry(130, None, Some(3), 210, None)
     *   LineSectionEntry(140, None, None, 250, Some(7))
     *   LineSectionEntry(160, None, Some(3), 300, Some(2)))
     * }}}
     *
     * encodes the following information:
     * {{{
     *   input line | output line begin | output line end
     *   -----------+-------------------+----------------
     *          123 |               207 |            207
     *          130 |               210 |            210
     *          131 |               211 |            211
     *          132 |               212 |            212
     *          140 |               250 |            256
     *          160 |               300 |            301
     *          161 |               302 |            303
     *          162 |               304 |            305
     * }}}
     *
     * @param inputStartLine Mapping input start.
     * @param lineFileId Mapping file ID.
     * @param repeatCount Starting from [[inputStartLine]], how many mappings are there?
     * @param outputStartLine Mapping output start.
     * @param outputLineIncrement Starting from [[outputStartLine]], how many lines are being mapped?
     */
    case class LineSectionEntry(inputStartLine: Int,
                                lineFileId: Option[Int] = None,
                                repeatCount: Option[Int] = None,
                                outputStartLine: Int,
                                outputLineIncrement: Option[Int] = None)

    /**
     * Case class modelling a JSR45 stratum.
     *
     * @param name The name of the stratum.
     * @param fileSection A sequence containing the file section entries.
     * @param lineSection A sequence containing the line section entries.
     * @param lineCount The number of lines mapped in this stratum.
     */
    case class Stratum(name: String,
                       fileSection: Seq[FileSectionEntry] = Seq.empty,
                       lineSection: Seq[LineSectionEntry] = Seq.empty,
                       lineCount: Int = 0) {
      final def findReverseLineMapping(line: Int): Option[RawLineMapping] = {
        lineSection.collectFirst {
          case LineSectionEntry(inputStartLine, lineFileId, repeatCount, outputStartLine, None)
              if outputStartLine until outputStartLine + repeatCount.getOrElse(1) contains line =>
            RawLineMapping(from = inputStartLine + (line - outputStartLine),
                           to = line,
                           sourceFileId = lineFileId)
          case LineSectionEntry(inputStartLine, lineFileId, None, outputStartLine, Some(outputLineInc))
              if outputStartLine until outputStartLine + outputLineInc contains line =>
            RawLineMapping(from = inputStartLine,
                           to = line,
                           sourceFileId = lineFileId)
          case LineSectionEntry(inputStartLine, lineFileId, Some(repeatCnt), outputStartLine, Some(outputLineInc))
              if outputStartLine until outputStartLine + repeatCnt*outputLineInc contains line =>
            RawLineMapping(from = inputStartLine + ((line - outputStartLine) / outputLineInc),
                           to = line,
                           sourceFileId = lineFileId)
        }
      }

      private def serializeFileSection: Seq[String] = {
        "*F" +:
          fileSection.zip(LazyList from 1).flatMap { entry =>
            val (FileSectionEntry(fileName, fileOrInternalPath), fileId) = entry
            Seq(Some(s"+ $fileId $fileName"), fileOrInternalPath)
          }.flatten
      }

      private def serializeLineSection: Seq[String] = {
        "*L" +:
          lineSection.map {
            case LineSectionEntry(inputStartLine, lineFileId, repeatCount, outputStartLine, outputLineIncrement) =>
              Seq(Some(inputStartLine.toString),
                  lineFileId.map(n => s"#$n"),
                  repeatCount.map(n => s",$n"),
                  Some(s":$outputStartLine"),
                  outputLineIncrement.map(n => s",$n")).flatten.mkString("")
          }
      }

      override def toString: String = {
        lineSection.headOption.map { _ =>
          (s"*S $name" +: (serializeFileSection ++ serializeLineSection)).mkString("\n")
        }.getOrElse("")
      }
    }

    case class DebugInfo(sourceFileName: String, strata: Seq[Stratum]) {
      val outputStrata: Seq[Stratum] = strata

      val header = Seq(
        "SMAP",         // this is an SMAP
        sourceFileName, // source file name for which this SMAP was generated
        "Scala"         // default stratum
      )

      val footer = Seq(
        "*E"            // end of the SMAP
      )

      override def toString: String = {
        (header ++ outputStrata.map(_.toString) ++ footer).mkString("\n")
      }
    }
  }

}