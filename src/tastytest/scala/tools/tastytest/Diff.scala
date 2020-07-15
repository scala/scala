package scala.tools.tastytest

import scala.jdk.CollectionConverters._

object Diff {
  def splitIntoLines(string: String): Seq[String] =
    string.trim.replace("\r\n", "\n").split("\n").toSeq

  def compareContents(output: String, check: String): String =
    compareContents(splitIntoLines(output), splitIntoLines(check))

  def compareContents(output: Seq[String], check: Seq[String]): String = {
    val diff = difflib.DiffUtils.diff(check.asJava, output.asJava)
    if (diff.getDeltas.isEmpty)
      ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          "check",
          "output",
          check.asJava,
          diff,
          1
        )
        .toArray()
        .mkString("\n")
  }
}
