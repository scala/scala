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

package scala.tools.tastytest

import scala.jdk.CollectionConverters._
import com.github.difflib.{DiffUtils, UnifiedDiffUtils}

object Diff {
  def removeTrailing(str: String): String = {
    val lastWhitespace = str.reverseIterator.indexWhere(!_.isWhitespace)
    if (lastWhitespace == -1) str
    else str.dropRight(lastWhitespace)
  }


  def splitIntoLines(string: String): Seq[String] =
    string.linesIterator.map(removeTrailing).toSeq

  def splitIntoLines(stream: java.util.stream.Stream[String]): Seq[String] =
    stream.map(removeTrailing).iterator().asScala.toSeq

  def compareContents(output: String, check: String): String =
    compareContents(splitIntoLines(output), splitIntoLines(check))

  def compareContents(output: Seq[String], check: Seq[String]): String = {
    val diff = DiffUtils.diff(check.asJava, output.asJava)
    if (diff.getDeltas.isEmpty)
      ""
    else
      UnifiedDiffUtils
        .generateUnifiedDiff(
          "check",
          "output",
          check.asJava,
          diff,
          1
        )
        .asScala
        .mkString("\n")
  }
}
