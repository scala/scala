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

  import DebugInfoBuilder.JSR45Info
  import DebugInfoBuilder.JSR45Stratum._

  import DebugInfoWriter._

  private[this] lazy val debugInfoWriters: LazyVar[mutable.Map[String, JSR45DebugInfoWriter]] = perRunLazy(this)(mutable.Map.empty)

  def registerCompUnitClasses(unit: GeneratedCompilationUnit): Unit = {
    for (cls <- unit.classes.filter(!_.isArtifact)) { // don't generate debug info for artifacts (e.g. mirror classes)
      debugInfoWriters.get += (cls.classNode.name -> new JSR45DebugInfoWriter(unit.sourceFile.name, cls))
    }
  }

  def writeDebugInfo(unit: GeneratedCompilationUnit): Unit = {
    for (cls <- unit.classes.filter(!_.isArtifact)) { // don't generate debug info for artifacts (e.g. mirror classes)
      val writer = debugInfoWriters.get(cls.classNode.name)
      cls.classNode.visitSource(writer.sourceFileName, writer.sourceDebugExtension())
    }
  }

  // Get the debug info writer for the specified callsite (class).
  def getWriter(fqCallsiteClass: String): DebugInfoWriter = debugInfoWriters.get.getOrElse(fqCallsiteClass, noOpDebugInfoWriter)

  sealed trait DebugInfoWriter {
    def addInlineLineInfo(callsiteLine: Int, inlineLine: Int, calleeFileName: String, calleeInternalName: String): Unit
  }

  object DebugInfoWriter {
    private[DebugInfoBuilder] val noOpDebugInfoWriter: DebugInfoWriter = new NoOpDebugInfoWriter

    class JSR45DebugInfoWriter(val sourceFileName: String, val cls: GeneratedClass) extends DebugInfoWriter {
      private val debugInfo = JSR45Info(sourceFileName,
                                        new ScalaStratum(cls.position.source.lineCount),
                                        new ScalaDebugStratum)

      debugInfo.scalaStratum.addFileEntry(FileSectionEntry(sourceFileName, Some(cls.classNode.name)))
      debugInfo.scalaDebugStratum.addFileEntry(FileSectionEntry(sourceFileName, Some(cls.classNode.name)))

      private def ensureFileEntry(entry: DebugInfoBuilder.JSR45Stratum.FileSectionEntry): Int = {
        val FileSectionEntry(fileName, _) = entry
        val foundFileEntryId = debugInfo.scalaStratum.getFileEntryId(fileName)
        if (foundFileEntryId == -1)
          debugInfo.scalaStratum.addFileEntry(entry) + 1
        else
          foundFileEntryId + 1
      }

      override def addInlineLineInfo(callsiteLine: Int,
                                     inlineLine: Int,
                                     calleeFileName: String,
                                     calleeInternalName: String): Unit = {
        val calleeFileId = ensureFileEntry(FileSectionEntry(calleeFileName, Some(calleeInternalName)))
        debugInfo.scalaStratum.registerLineForFile(inlineLine, calleeFileId)
        debugInfo.scalaDebugStratum.addRawLineMapping(RawLineMapping(from = callsiteLine,
                                                                     toStart = inlineLine,
                                                                     toEnd = inlineLine,
                                                                     sourceFileId = Some(1)))
      }

      def sourceDebugExtension(): String = debugInfo.toString
    }

    class NoOpDebugInfoWriter extends DebugInfoWriter {
      override def addInlineLineInfo(callsiteLine: Int,
                                     inlineLine: Int,
                                     calleeFileName: String,
                                     calleeInternalName: String): Unit = () // noop
    }
  }
}

object DebugInfoBuilder {

  import JSR45Stratum._

  sealed abstract class JSR45Stratum(val name: String) extends Serializable {
    protected var fileSection: Seq[FileSectionEntry] = Seq.empty
    protected var lineMapping: Seq[RawLineMapping]   = Seq.empty

    // Add a new FileSectionEntry to the fileSection.
    // Return the ID of the entry just added.
    def addFileEntry(entry: FileSectionEntry): Int = {
      fileSection :+= entry
      fileSection.length - 1
    }

    // Given a fileName, return the ID corresponding to that file section
    // entry, or -1 if no such an entry exists.
    def getFileEntryId(fileName: String): Int = {
      fileSection.indexWhere(_.fileName == fileName)
    }

    def addRawLineMapping(rawLineMapping: RawLineMapping): Unit = {
      lineMapping :+= rawLineMapping
    }

    // compute the line section from the line mappings
    private def lineSection: Seq[LineSectionEntry] = {
      val sortedLineMapping = lineMapping.distinct.sorted
      sortedLineMapping.headOption.map {
        case RawLineMapping(from, toStart, toEnd, sourceFileId) =>
          val start = LineSectionEntry(inputStartLine = from,
                                       lineFileId = sourceFileId,
                                       outputStartLine = toStart,
                                       outputLineIncrement = if (toStart != toEnd) Some(toEnd - toStart + 1) else None)
          sortedLineMapping.tail.foldLeft(Seq(start)) {
            case (soFar :+ last, RawLineMapping(from, toStart, toEnd, sourceFileId)) if toStart == toEnd =>
              val lastRepeatCount = last.repeatCount.getOrElse(1)
              if (last.lineFileId == sourceFileId &&
                  last.inputStartLine + lastRepeatCount == from &&
                  last.outputStartLine + lastRepeatCount == toEnd)
                soFar :+ last.copy(repeatCount = Some(last.repeatCount.getOrElse(1) + 1))
              else
                soFar :+ last :+ LineSectionEntry(inputStartLine = from, lineFileId = sourceFileId, outputStartLine = toStart)
            case (soFar :+ last, RawLineMapping(from, toStart, toEnd, sourceFileId)) if toStart < toEnd  =>
              val lastRepeatCount = last.repeatCount.getOrElse(1)
              if (last.lineFileId == sourceFileId &&
                  last.inputStartLine + lastRepeatCount == from &&
                  (toEnd - toStart + 1) == last.outputLineIncrement.getOrElse(0))
                soFar :+ last.copy(repeatCount = Some(last.repeatCount.getOrElse(1) + 1))
              else
                soFar :+ last :+ LineSectionEntry(inputStartLine = from,
                                                  lineFileId = sourceFileId,
                                                  outputStartLine = toStart,
                                                  outputLineIncrement = Some(toEnd - toStart + 1))
          }
      }.getOrElse(Seq.empty)
    }

    def fileSectionLines: Seq[String] =
      "*F" +:
        fileSection.zipWithIndex.flatMap {
          case (FileSectionEntry(fileName, Some(absoluteFileName)), id) =>
            Seq(s"+ ${id + 1} $fileName", s"$absoluteFileName")
          case (FileSectionEntry(fileName, None), id)                   =>
            Seq(s"${id + 1} $fileName")
        }

    // this method triggers line section computation from the line mappings
    def lineSectionLines: Seq[String] =
      "*L" +:
        lineSection.map { lineSection =>
          val LineSectionEntry(inputStartLine, lineFileId, repeatCount, outputStartLine, outputLineIncrement) = lineSection

          val builder = new StringBuilder
          builder.append(inputStartLine)
          lineFileId.map(id => builder.append(s"#$id"))
          repeatCount.map(cnt => builder.append(s",$cnt"))
          builder.append(s":$outputStartLine")
          outputLineIncrement.map(inc => builder.append(s",$inc"))

          builder.toString
        }

    def toStringLines: Seq[String] =
      if (lineMapping.nonEmpty)
        s"*S $name" +: (fileSectionLines ++ lineSectionLines) :+ "*E"
      else
        Seq.empty

  }

  object JSR45Stratum {

    // The "Scala" stratum acts as a catalogue for all the source lines that the corresponding source contains.
    // The catalogue starts with the lines of the source itself.
    // Any lines from inline methods are added to the end of the catalogue, as inline requests are served.
    // These lines are called "artificial" lines, because they don't actually come from the original source.
    final class ScalaStratum(sourceLineCount: Int) extends JSR45Stratum("Scala") {

      for (line <- 1 to sourceLineCount) {
        addRawLineMapping(RawLineMapping(line, line, line, sourceFileId = Some(1)))
      }

      private var lineOffset = sourceLineCount + 1

      // In the "Scala" stratum, a line from a given source cannot be mapped to more than 1 locations.
      def registerLineForFile(line: Int, sourceFileId: Int): Unit = {
        lineMapping.find(m => (m.sourceFileId.get, m.from) == (sourceFileId, line)) match {
          case None =>
            addRawLineMapping(RawLineMapping(from = line,
                                             toStart = lineOffset,
                                             toEnd = lineOffset,
                                             sourceFileId = Some(sourceFileId)))
            lineOffset += 1
          case _ =>
        }
      }
    }

    final class ScalaDebugStratum extends JSR45Stratum("ScalaDebug")

    case class FileSectionEntry(fileName: String,
                                absoluteFileName: Option[String] = None)

    case class RawLineMapping(from: Int,
                              toStart: Int,
                              toEnd: Int,
                              sourceFileId: Option[Int] = None) extends Ordered[RawLineMapping] {
      override def compare(that: RawLineMapping): Int =
        implicitly[Ordering[(Int, Int, Int)]].compare((this.sourceFileId.getOrElse(0), this.from, this.toStart),
                                                      (that.sourceFileId.getOrElse(0), that.from, that.toStart))
    }

    case class LineSectionEntry(inputStartLine: Int,
                                lineFileId: Option[Int] = None,
                                repeatCount: Option[Int] = None,
                                outputStartLine: Int,
                                outputLineIncrement: Option[Int] = None)

  }

  case class JSR45Info(source: String,
                       scalaStratum: JSR45Stratum.ScalaStratum,
                       scalaDebugStratum: JSR45Stratum.ScalaDebugStratum) {
    def strata: Seq[JSR45Stratum] = Seq(scalaStratum, scalaDebugStratum)

    override def toString: String = {
      (Seq("SMAP", source) ++ strata.flatMap(_.toStringLines)).mkString("\n")
    }
  }

  case class JSR45MalformedDebugInfo() extends Exception("")

}
