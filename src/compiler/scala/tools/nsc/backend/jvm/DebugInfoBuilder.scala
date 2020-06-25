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

  private[this] lazy val debugInfoWriters: LazyVar[mutable.Map[GeneratedClass, JSR45Writer]] = perRunLazy(this)(mutable.Map.empty)

  def registerUnitClasses(unit: GeneratedCompilationUnit): Unit = {
    for (cls <- unit.classes.filter(!_.isArtifact)) { // don't generate debug info for artifacts (e.g. mirror classes)
      debugInfoWriters.get += (cls -> new JSR45Writer(unit.sourceFile.name, cls))
    }
  }

  def writeDebugInfo(unit: GeneratedCompilationUnit): Unit = {
    for (cls <- unit.classes.filter(!_.isArtifact)) { // don't generate debug info for artifacts (e.g. mirror classes)
      val writer = debugInfoWriters.get(cls)
      cls.classNode.visitSource(writer.sourceFileName, writer.sourceDebugExtension())
    }
  }

  def getWriter(cls: GeneratedClass): JSR45Writer = debugInfoWriters.get(cls)

  class JSR45Writer(val sourceFileName: String, val cls: GeneratedClass) {
    private val debugInfo = JSR45Info(sourceFileName, new ScalaStratum, new ScalaDebugStratum)

    debugInfo.scalaStratum.addFileEntry(FileSectionEntry(sourceFileName, Some(cls.classNode.name)))
    for (line <- 1 to cls.position.source.lineCount) {
      debugInfo.scalaStratum.addRawLineMapping(RawLineMapping(line, line, line))
    }

    def sourceDebugExtension(): String = debugInfo.toString
  }

}

object DebugInfoBuilder {

  import JSR45Stratum._

  sealed abstract class JSR45Stratum(val name: String) extends Serializable {
    private var fileSection: Seq[FileSectionEntry] = Seq.empty
    private var lineMapping: Seq[RawLineMapping]   = Seq.empty

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
      val sortedLineMapping = lineMapping.sorted
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
      if (lineSection.nonEmpty)
        s"*S $name" +: (fileSectionLines ++ lineSectionLines) :+ "*E"
      else
        Seq.empty

  }

  object JSR45Stratum {

    final class ScalaStratum extends JSR45Stratum("Scala")

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
