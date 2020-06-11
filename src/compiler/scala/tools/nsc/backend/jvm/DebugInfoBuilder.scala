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

package scala.tools.nsc.backend.jvm

abstract class DebugInfoBuilder extends BCodeHelpers {

  import global._

  class JSR45Builder(cunit: CompilationUnit) {}

}

object DebugInfoBuilder {

  import JSR45Stratum._

  sealed abstract class JSR45Stratum(val name: String) extends Serializable {
    private var fileSection: Seq[FileSectionEntry]  = Seq.empty
    private var lineMapping: Seq[(Int, (Int, Int))] = Seq.empty

    def addFileEntry(entry: FileSectionEntry): Unit = {
      fileSection :+= entry
    }

    def addLineMapping(inputStartLine: Int, outputStartLine: Int): Unit = {
      addLineMapping(inputStartLine, outputStartLine, outputStartLine)
    }

    def addLineMapping(inputStartLine: Int,
                       outputStartLine: Int,
                       outputEndLine: Int): Unit = {
      lineMapping :+= ((inputStartLine, (outputStartLine, outputEndLine)))
    }

    // compute the line section from the line mappings
    private def lineSection: Seq[LineSectionEntry] = {
      val sortedLineMapping = lineMapping.sorted
      sortedLineMapping.headOption.map {
        case (from, (toStart, toEnd)) =>
          val start = LineSectionEntry(inputStartLine = from,
                                       outputStartLine = toStart,
                                       outputLineIncrement = if (toStart != toEnd) Some(toEnd - toStart + 1) else None)
          sortedLineMapping.tail.foldLeft(Seq(start)) {
            case (soFar :+ last, (from, (toStart, toEnd))) if toStart == toEnd =>
              val lastRepeatCount = last.repeatCount.getOrElse(1)
              if (last.inputStartLine + lastRepeatCount == from && last.outputStartLine + lastRepeatCount == toEnd)
                soFar :+ last.copy(repeatCount = Some(last.repeatCount.getOrElse(1) + 1))
              else
                soFar :+ last :+ LineSectionEntry(inputStartLine = from, outputStartLine = toStart)
            case (soFar :+ last, (from, (toStart, toEnd))) if toStart < toEnd  =>
              val lastRepeatCount = last.repeatCount.getOrElse(1)
              if (last.inputStartLine + lastRepeatCount == from && (toEnd - toStart + 1) == last.outputLineIncrement.getOrElse(0))
                soFar :+ last.copy(repeatCount = Some(last.repeatCount.getOrElse(1) + 1))
              else
                soFar :+ last :+ LineSectionEntry(inputStartLine = from, outputStartLine = toStart, outputLineIncrement = Some(toEnd - toStart + 1))
          }
      }.getOrElse(Seq.empty)
    }

    def fileSectionLines: Seq[String] =
      "*F" +:
        fileSection.zipWithIndex.flatMap {
          case (FileSectionEntry(fileName, Some(absoluteFileName)), id) =>
            Seq(s"+ ${id+1} $fileName", s"$absoluteFileName")
          case (FileSectionEntry(fileName, None), id)                   =>
            Seq(s"${id+1} $fileName")
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
      s"*S $name" +: (fileSectionLines ++ lineSectionLines) :+ "*E"

  }

  object JSR45Stratum {

    final class ScalaStratum extends JSR45Stratum("Scala")

    final class ScalaDebugStratum extends JSR45Stratum("ScalaDebug")

    case class FileSectionEntry(fileName: String,
                                absoluteFileName: Option[String] = None)

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
