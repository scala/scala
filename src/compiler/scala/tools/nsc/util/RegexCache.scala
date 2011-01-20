/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc
package util
import java.util.regex.Pattern
import scala.collection.mutable

object RegexCache {
  /** Maps patterns to compiled regexes */
  private val regexMap = mutable.Map.empty[String, Pattern]

  /** Lists the regexes that have been recorded in order */
  private val regexList = new mutable.Queue[String]

  private val regexesToCache = 1000

  /** Compile a regex and add it to the cache */
  private def compileAndAdd(regex: String): Pattern = {
    val pattern = Pattern.compile(regex)

    regexMap += (regex -> pattern)
    regexList += regex

    if (regexMap.size > regexesToCache)
      regexMap -= regexList.dequeue()

    pattern
  }


  /** Compile a regex, caching */
  def apply(regex: String): Pattern =
    regexMap.get(regex) match {
      case Some(pattern) => pattern
      case None => compileAndAdd(regex)
    }
}
