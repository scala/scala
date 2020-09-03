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

/**
 * Constructs and writes the JSR-45 debug information for the generated class files.
 *   http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.11
 *   https://jcp.org/aboutJava/communityprocess/final/jsr045/index.html
 *
 * Here is an overview of the constructed information.
 *
 * For each class file, we generate two JSR-45 strata, namely "Scala" and "ScalaDebug".
 *
 * - The "Scala" stratum:
 *   The Scala stratum acts as a linear catalogue, containing references to the lines
 *   of code that the class contains.
 *
 *   It provides the following information:
 *   * From which source files does this class file contain code?
 *   * To which lines from each source file may this class refer to?
 *
 *   The format is the following:
 *   * For a source file containing N lines, the first N lines of the stratum will be those.
 *   * With every inline request satisfied, the lines that were inline will be placed at the
 *     end of the catalogue.
 *
 *   Ultimately, the result is a linear catalogue with all the lines from the various source
 *   files that the class will need to refer to when debugging.
 *
 * - The "ScalaDebug" stratum:
 *   The ScalaDebug stratum acts as an index, providing the following information:
 *   * For each line of code in the original file, which are the external lines
 *     that are inline due to that line?
 *
 *   Essentially, the ScalaDebug stratum provides a trace of the inlining requests to the original
 *   source code. This allows the debuggers to trace to the potentially external files, from which
 *   code was inline.
 *
 *   Here is an example. Assume that we have the following two files.
 *
 *   Main.scala:
 * {{{
 *    1. // Main.scala
 *    2. // some comment here...
 *    3. import Utils._
 *    4.
 *    5. class Main {
 *    6.   def foo(): Unit = {
 *    7.     val myList = List(1, 2, 3, 4, 50, 60, 70, 80)
 *    8.     val bmList = myList.map(blackMagic)
 *    9.   }
 *   10. }
 *   11. [empty line]
 *}}}
 *   Utils.scala:
 *{{{
 *    1. object Utils {
 *    2.  @inline def blackMagic(n: Int): Int = {
 *    3.    if (n < 10)
 *    4.      2 * n
 *    5.    else
 *    6.          n
 *    7.  }
 *    8. }
 *    9. [empty line]
 *}}}
 *   Then the debug information that will be generated for the Main class is the following:
 *{{{
 *   SMAP
 *   Main.scala
 *   Scala
 *   *S Scala
 *   *F
 *   + 1 Main.scala
 *   Main
 *   + 2 List.scala
 *   scala/collection/immutable/List$
 *   + 3 Utils.scala
 *   Utils$
 *   *L
 *   1#1,11:1
 *   648#2:12
 *   245#2,9:13
 *   249#2:22
 *   255#2,2:23
 *   3#3,2:25
 *   6#3:27
 *   3#3,2:28
 *   6#3:30
 *   *S ScalaDebug
 *   *F
 *   + 1 Main.scala
 *   Main
 *   *L
 *   7#1:12
 *   8#1:13,12
 *   8#1:25,3
 *   8#1:28,3
 *   *E
 *}}}
 *   The SMAP starts with the source file name (in our case Main.scala) and then defines two strata,
 *   indicated by the *S prefix. As discussed earlier, those are Scala and ScalaDebug.
 *
 *   Each stratum first indicates the source files from where it contains code references.
 *   Those are defined in the file section (*F). The first file of each file section entry
 *   contains the file ID, the source file name and the internal class name in the classpath.
 *
 *   Then the line section follows. Because the Main.scala file contains 11 lines, the first
 *   11 lines in the catalogue are filled by those lines. This is achieved via the mapping:
 *{{{
 *   1#1,11:1
 *}}}
 *   which says: "starting from line 1 of file ID 1, map 11 lines starting from line 1 of the catalogue"
 *
 *   This creates the mapping:
 *{{{
 *    1 ->  1
 *    2 ->  2
 *    3 ->  3
 *     ...
 *   11 -> 11
 *}}}
 *   Then it creates the mapping for some calls to the List collection and then the mapping for the inline
 *   method from the Utils module, which has file ID = 3.
 *{{{
 *   3#3,2:24
 *   6#3:26
 *}}}
 *   This creates the following mapping for the inline lines from the Utils module:
 *{{{
 *    3 -> 25
 *    4 -> 26
 *    6 -> 27
 *}}}
 *   Notice how line 5 (containing only the "if" keyword did not make it to the catalogue).
 *
 *   The ScalaDebug stratum contains three line mappings:
 *{{{
 *   7#1:12
 *   8#1:13,12
 *   8#1:25,3
 *}}}
 *   What those mappings indicate is that:
 *   * line 12 on the catalogue is inline from line 7 of the original source file
 *     (that is the List.apply method)
 *   * lines [13-24] on the catalogue are inline to line 8 of the original source file
 *     (those are some of the List.map method lines)
 *   * lines [25-27] on the catalogue are inline to line 8 of the original source file
 *     (those are the Utils.blackMagic function lines)
 *
 *   Notice that the two last lines of the ScalaDebug stratum could be compressed into one,
 *   however since their mappings belong to different external files, we keep them separated.
 *
 *   The debugger can exploit that information in order to more accurately trace through the code
 *   in the presence of inline methods. In order to further help with that, the Scala stratum keeps
 *   inline requests separate.
 *
 *   For instance, in the above example, mapping:
 *{{{
 *   245#2,9:13
 *}}}
 *   also contains line 249, however we still need the following mapping as well:
 *{{{
 *   249#2:22
 *}}}
 *   because line 249 was inline anew with a separate inline request.
 *
 *   Please note that, an inline line from an external file may be in turn inline from another
 *   external file. In this case, the debugger needs to figure that out and trace to the origin
 *   of that line.
 */
abstract class DebugInfoBuilder extends PerRunInit {
  val postProcessor: PostProcessor

  import postProcessor.bTypes
  import bTypes.{LazyVar, perRunLazy}

  import DebugInfoBuilder.JSR45Info
  import DebugInfoBuilder.JSR45Stratum._

  import DebugInfoWriter._

  private[this] lazy val debugInfoWriters: LazyVar[mutable.Map[String, DebugInfoWriter]] = perRunLazy(this)(mutable.Map.empty)

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
    val sourceFileName: String

    val scalaStratum: ScalaStratum

    /**
     * Add a mapping for an inline line from an external source to this class source.
     *
     * @param callsiteLine the line in the class source that made the inline request.
     * @param inlineLine the external source line that will be inline.
     * @param calleeFileName the file name of the external source.
     * @param calleeInternalName the internal name of the external source.
     */
    def addInlineLineInfo(callsiteLine: Int, inlineLine: Int, calleeFileName: String, calleeInternalName: String): Int

    /**
     *
     * @param line
     * @return
     */
    def getRecursiveInlineInfo(calleeInternalName: String, line: Int): Option[(FileSectionEntry, Int)]

    /**
     * Separators are used so that different inline requests which overlap in the line numbers are
     * kept separate in the final SMAP. This information must be preserved, as it will help the
     * debugger trace through the inline code easier.
     */
    def addSeparator(): Unit

    /**
     * Computes the final SMAP string from the collected information.
     * Triggers compression of the information.
     *
     * @return the String that should be written to the SourceDebugExtension class attribute.
     */
    def sourceDebugExtension(): String
  }

  object DebugInfoWriter {
    private[DebugInfoBuilder] val noOpDebugInfoWriter: DebugInfoWriter = new NoOpDebugInfoWriter

    class JSR45DebugInfoWriter(override val sourceFileName: String, val cls: GeneratedClass) extends DebugInfoWriter {
      private val debugInfo = JSR45Info(sourceFileName,
                                        new ScalaStratum(cls.position.source.lineCount),
                                        new ScalaDebugStratum)

      override val scalaStratum: ScalaStratum = debugInfo.scalaStratum

      debugInfo.scalaStratum.addFileEntry(FileSectionEntry(sourceFileName, Some(cls.classNode.name)))
      debugInfo.scalaDebugStratum.addFileEntry(FileSectionEntry(sourceFileName, Some(cls.classNode.name)))

      private def ensureFileEntry(entry: DebugInfoBuilder.JSR45Stratum.FileSectionEntry): Int = {
        val foundFileEntryId = debugInfo.scalaStratum.getFileEntryId(entry)
        if (foundFileEntryId == -1)
          debugInfo.scalaStratum.addFileEntry(entry) + 1
        else
          foundFileEntryId + 1
      }

      override def addInlineLineInfo(callsiteLine: Int,
                                     inlineLine: Int,
                                     calleeFileName: String,
                                     calleeInternalName: String): Int = {
        val (fileEntry, line) = getRecursiveInlineInfo(calleeInternalName, inlineLine) match {
          case Some(info) => info
          case None => (FileSectionEntry(calleeFileName, Some(calleeInternalName)), inlineLine)
        }

        val calleeFileId = ensureFileEntry(fileEntry)
        val inlineLinePos = debugInfo.scalaStratum.registerLineForFile(line, calleeFileId)

        debugInfo.scalaDebugStratum.addRawLineMapping(RawLineMapping(from = callsiteLine,
                                                                     toStart = inlineLinePos,
                                                                     toEnd = inlineLinePos,
                                                                     sourceFileId = Some(1)))
        inlineLinePos
      }

      override def getRecursiveInlineInfo(calleeInternalName: String, line: Int): Option[(FileSectionEntry, Int)] = {
        debugInfoWriters.get.get(calleeInternalName).flatMap { writer =>
          writer.scalaStratum.findReverseLineMapping(line).flatMap {
            case (lineMapping, fileEntry) if lineMapping.from != lineMapping.toStart =>
              fileEntry.absoluteFileName.flatMap(getRecursiveInlineInfo(_, lineMapping.from))
            case (lineMapping, fileEntry) =>
              Some((fileEntry, lineMapping.from))
          }
        }
      }

      override def addSeparator(): Unit = {
        debugInfo.scalaDebugStratum.addRawLineMapping(RawLineMapping(-1, -1, -1, Some(-1)))
      }

      override def sourceDebugExtension(): String = debugInfo.toString
    }

    /**
     * The NoOpDebugInfoWriter is needed in case debug info output is turned off.
     *
     * All calls in this class are considered to be no-ops.
     * The sourceDebugExtension() method returns null.
     */
    class NoOpDebugInfoWriter extends DebugInfoWriter {
      override val sourceFileName: String = null

      override val scalaStratum: ScalaStratum = null

      override def addInlineLineInfo(callsiteLine: Int,
                                     inlineLine: Int,
                                     calleeFileName: String,
                                     calleeInternalName: String): Int = -1 // noop

      override def getRecursiveInlineInfo(calleeInternalName: String, line: Int): Option[(FileSectionEntry, Int)] = None // noop

      override def addSeparator(): Unit = () // noop

      override def sourceDebugExtension(): String = null
    }
  }
}

object DebugInfoBuilder {

  import JSR45Stratum._

  sealed abstract class JSR45Stratum(val name: String) extends Serializable {
    protected val fileSection: mutable.ArrayBuffer[FileSectionEntry] = mutable.ArrayBuffer.empty
    protected val lineMapping: mutable.ArrayBuffer[RawLineMapping]   = mutable.ArrayBuffer.empty

    // Add a new FileSectionEntry to the fileSection.
    // Return the ID of the entry just added.
    def addFileEntry(entry: FileSectionEntry): Int = {
      fileSection.append(entry)
      fileSection.length - 1
    }

    // Given a fileName, return the ID corresponding to that file section
    // entry, or -1 if no such an entry exists.
    def getFileEntryId(entry: FileSectionEntry): Int = {
      fileSection.indexOf(entry)
    }

    def addRawLineMapping(rawLineMapping: RawLineMapping): Unit = {
      lineMapping.append(rawLineMapping)
    }

    def findReverseLineMapping(line: Int): Option[(RawLineMapping, FileSectionEntry)] = {
      val maybeMapping = lineMapping.find(_.toStart == line)
      maybeMapping.flatMap { mapping =>
        mapping.sourceFileId.map { fileId =>
          (mapping, fileSection(fileId - 1))
        }
      }
    }

    // compute the line section from the line mappings
    private def lineSection: Seq[LineSectionEntry] = {
      lineMapping.headOption.map {
        case RawLineMapping(from, toStart, toEnd, sourceFileId) =>
          val start = LineSectionEntry(inputStartLine = from,
                                       lineFileId = sourceFileId,
                                       outputStartLine = toStart,
                                       outputLineIncrement = if (toStart != toEnd) Some(toEnd - toStart + 1) else None)
          lineMapping.tail.foldLeft(Seq(start)) {
            case (soFar, RawLineMapping(-1, -1, -1, Some(-1))) => // transfer separator
              soFar :+ LineSectionEntry(-1, Some(-1), Some(-1), outputStartLine = -1, Some(-1))
            case (soFar :+ last, RawLineMapping(from, toStart, toEnd, sourceFileId)) if toStart == toEnd =>
              val lastRepeatCount = last.repeatCount.getOrElse(1)
              val lastOutputLineIncrement = last.outputLineIncrement.getOrElse(1)
              if (last.lineFileId == sourceFileId &&
                  last.inputStartLine + lastRepeatCount == from &&
                  last.outputStartLine + lastRepeatCount == toEnd)
                soFar :+ last.copy(repeatCount = Some(last.repeatCount.getOrElse(1) + 1))
              else if (last.lineFileId == sourceFileId &&
                       last.inputStartLine == from &&
                       (last.outputStartLine + lastOutputLineIncrement) == toStart)
                 soFar :+ last.copy(outputLineIncrement = Some(lastOutputLineIncrement + 1))
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
        }.toSeq

    // this method triggers line section computation from the line mappings
    def lineSectionLines: Seq[String] = {
      val builder = new StringBuilder(new java.lang.StringBuilder(512))

      "*L" +:
        lineSection
          .filter {
            // skip separators
            case LineSectionEntry(-1, Some(-1), Some(-1), -1, Some(-1)) => false
            case _ => true
          }
          .map { lineSection =>
            val LineSectionEntry(inputStartLine, lineFileId, repeatCount, outputStartLine, outputLineIncrement) = lineSection

            builder.clear()
            builder.append(inputStartLine)
            lineFileId.foreach(id => builder.append(s"#$id"))
            repeatCount.foreach(cnt => builder.append(s",$cnt"))
            builder.append(s":$outputStartLine")
            outputLineIncrement.foreach(inc => builder.append(s",$inc"))

            builder.toString
          }
    }

    def toStringLines: Seq[String] =
      if (lineMapping.nonEmpty)
        s"*S $name" +: (fileSectionLines ++ lineSectionLines)
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

      def registerLineForFile(line: Int, sourceFileId: Int): Int = {
        val mapping = lineOffset
        addRawLineMapping(RawLineMapping(from = line,
                                         toStart = mapping,
                                         toEnd = mapping,
                                         sourceFileId = Some(sourceFileId)))
        lineOffset += 1
        mapping
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
    def outputStrata: Seq[JSR45Stratum] = Seq(scalaStratum)

    override def toString: String = {
      val header = Seq(
        "SMAP",  // this is an SMAP
        source,  // source file name for which this SMAP was generated
        "Scala"  // default stratum
      )
      val footer = Seq("*E")
      (header ++ outputStrata.flatMap(_.toStringLines) ++ footer).mkString("\n")
    }
  }

  case class JSR45MalformedDebugInfo() extends Exception("Malformed JSR-45 debug info (SourceDebugExtension)")

}
