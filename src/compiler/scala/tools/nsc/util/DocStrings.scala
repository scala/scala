/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package util

import scala.reflect.internal.Chars._

/** Utility methods for doc comment strings
 */
object DocStrings {

  /** Returns index of string `str` following `start` skipping longest
   *  sequence of whitespace characters characters (but no newlines)
   */
  def skipWhitespace(str: String, start: Int): Int =
    if (start < str.length && isWhitespace(str charAt start)) skipWhitespace(str, start + 1)
    else start

  /** Returns index of string `str` following `start` skipping
   *  sequence of identifier characters.
   */
  def skipIdent(str: String, start: Int): Int =
    if (start < str.length && isIdentifierPart(str charAt start)) skipIdent(str, start + 1)
    else start

  /** Returns index of string `str` following `start` skipping
   *  sequence of identifier characters.
   */
  def skipTag(str: String, start: Int): Int =
    if (start < str.length && (str charAt start) == '@') skipIdent(str, start + 1)
    else start


  /** Returns index of string `str` after `start` skipping longest
   *  sequence of space and tab characters, possibly also containing
   *  a single `*` character or the `/``**` sequence.
   *  @pre  start == str.length || str(start) == `\n`
   */
  def skipLineLead(str: String, start: Int): Int =
    if (start == str.length) start
    else {
      val idx = skipWhitespace(str, start + 1)
      if (idx < str.length && (str charAt idx) == '*') skipWhitespace(str, idx + 1)
      else if (idx + 2 < str.length && (str charAt idx) == '/' && (str charAt (idx + 1)) == '*' && (str charAt (idx + 2)) == '*')
        skipWhitespace(str, idx + 3)
      else idx
    }

  /** Skips to next occurrence of `\n` or to the position after the `/``**` sequence following index `start`.
   */
  def skipToEol(str: String, start: Int): Int =
    if (start + 2 < str.length && (str charAt start) == '/' && (str charAt (start + 1)) == '*' && (str charAt (start + 2)) == '*') start + 3
    else if (start < str.length && (str charAt start) != '\n') skipToEol(str, start + 1)
    else start

  /** Returns first index following `start` and starting a line (i.e. after skipLineLead) or starting the comment
   *  which satisfies predicate `p`.
   */
  def findNext(str: String, start: Int)(p: Int => Boolean): Int = {
    val idx = skipLineLead(str, skipToEol(str, start))
    if (idx < str.length && !p(idx)) findNext(str, idx)(p)
    else idx
  }

  /** Return first index following `start` and starting a line (i.e. after skipLineLead)
   *  which satisfies predicate `p`.
   */
  def findAll(str: String, start: Int)(p: Int => Boolean): List[Int] = {
    val idx = findNext(str, start)(p)
    if (idx == str.length) List()
    else idx :: findAll(str, idx)(p)
  }

  /** Produces a string index, which is a list of `sections`, i.e
   *  pairs of start/end positions of all tagged sections in the string.
   *  Every section starts with an at sign and extends to the next at sign,
   *  or to the end of the comment string, but excluding the final two
   *  characters which terminate the comment.
   *
   *  Also take usecases into account - they need to expand until the next
   *  usecase or the end of the string, as they might include other sections
   *  of their own
   */
  def tagIndex(str: String, p: Int => Boolean = (idx => true)): List[(Int, Int)] = {
    var indices = findAll(str, 0) (idx => str(idx) == '@' && p(idx))
    indices = mergeUsecaseSections(str, indices)
    indices = mergeInheritdocSections(str, indices)

    indices match {
      case List() => List()
      case idxs   => idxs zip (idxs.tail ::: List(str.length - 2))
    }
  }

  /**
   * Merge sections following an usecase into the usecase comment, so they
   * can override the parent symbol's sections
   */
  def mergeUsecaseSections(str: String, idxs: List[Int]): List[Int] = {
    idxs.indexWhere(str.startsWith("@usecase", _)) match {
      case firstUCIndex if firstUCIndex != -1 =>
        val commentSections = idxs.take(firstUCIndex)
        val usecaseSections = idxs.drop(firstUCIndex).filter(str.startsWith("@usecase", _))
        commentSections ::: usecaseSections
      case _ =>
        idxs
    }
  }

  /**
   * Merge the inheritdoc sections, as they never make sense on their own
   */
  def mergeInheritdocSections(str: String, idxs: List[Int]): List[Int] =
    idxs.filterNot(str.startsWith("@inheritdoc", _))

  /** Does interval `iv` start with given `tag`?
   */
  def startsWithTag(str: String, section: (Int, Int), tag: String): Boolean =
    startsWithTag(str, section._1, tag)

  def startsWithTag(str: String, start: Int, tag: String): Boolean =
    str.startsWith(tag, start) && !isIdentifierPart(str charAt (start + tag.length))

  /** The first start tag of a list of tag intervals,
   *  or the end of the whole comment string - 2 if list is empty
   */
  def startTag(str: String, sections: List[(Int, Int)]) = sections match {
    case Nil             => str.length - 2
    case (start, _) :: _ => start
  }

  /** A map from parameter names to start/end indices describing all parameter
   *  sections in `str` tagged with `tag`, where `sections` is the index of `str`.
   */
  def paramDocs(str: String, tag: String, sections: List[(Int, Int)]): Map[String, (Int, Int)] =
    Map() ++ {
      for (section <- sections if startsWithTag(str, section, tag)) yield {
        val start = skipWhitespace(str, section._1 + tag.length)
        str.substring(start, skipIdent(str, start)) -> section
      }
    }

  /** Optionally start and end index of return section in `str`, or `None`
   *  if `str` does not have a @group. */
  def groupDoc(str: String, sections: List[(Int, Int)]): Option[(Int, Int)] =
    sections find (startsWithTag(str, _, "@group"))


  /** Optionally start and end index of return section in `str`, or `None`
   *  if `str` does not have a @return.
   */
  def returnDoc(str: String, sections: List[(Int, Int)]): Option[(Int, Int)] =
    sections find (startsWithTag(str, _, "@return"))

  /** Extracts variable name from a string, stripping any pair of surrounding braces */
  def variableName(str: String): String =
    if (str.length >= 2 && (str charAt 0) == '{' && (str charAt (str.length - 1)) == '}')
      str.substring(1, str.length - 1)
    else
      str

  /** Returns index following variable, or start index if no variable was recognized
   */
  def skipVariable(str: String, start: Int): Int = {
    var idx = start
    if (idx < str.length && (str charAt idx) == '{') {
      do idx += 1
      while (idx < str.length && (str charAt idx) != '}')
      if (idx < str.length) idx + 1 else start
    } else {
      while (idx < str.length && isVarPart(str charAt idx))
        idx += 1
      idx
    }
  }

  /** A map from the section tag to section parameters */
  def sectionTagMap(str: String, sections: List[(Int, Int)]): Map[String, (Int, Int)] =
    Map() ++ {
      for (section <- sections) yield
        extractSectionTag(str, section) -> section
    }

  /** Extract the section tag, treating the section tag as an identifier */
  def extractSectionTag(str: String, section: (Int, Int)): String =
    str.substring(section._1, skipTag(str, section._1))

  /** Extract the section parameter */
  def extractSectionParam(str: String, section: (Int, Int)): String = {
    val (beg, _) = section
    assert(str.startsWith("@param", beg) ||
           str.startsWith("@tparam", beg) ||
           str.startsWith("@throws", beg))

    val start = skipWhitespace(str, skipTag(str, beg))
    val finish = skipIdent(str, start)

    str.substring(start, finish)
  }

  /** Extract the section text, except for the tag and comment newlines */
  def extractSectionText(str: String, section: (Int, Int)): (Int, Int) = {
    val (beg, end) = section
    if (str.startsWith("@param", beg) ||
        str.startsWith("@tparam", beg) ||
        str.startsWith("@throws", beg))
      (skipWhitespace(str, skipIdent(str, skipWhitespace(str, skipTag(str, beg)))), end)
    else
      (skipWhitespace(str, skipTag(str, beg)), end)
  }

  /** Cleanup section text */
  def cleanupSectionText(str: String) = {
    var result = str.trim.replaceAll("\n\\s+\\*\\s+", " \n")
    while (result.endsWith("\n"))
      result = result.substring(0, str.length - 1)
    result
  }

}
