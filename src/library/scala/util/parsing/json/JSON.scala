/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.parsing.json

/**
 * This object provides a simple interface to the JSON parser class.
 * The default conversion for numerics is into a double. If you wish to
 * override this behavior at the global level, you can set the
 * `globalNumberParser` property to your own `(String => Any)` function.
 * If you only want to override at the per-thread level then you can set
 * the `perThreadNumberParser` property to your function. For example:
 * {{{
 * val myConversionFunc = {input : String => BigDecimal(input)}
 *
 * // Global override
 * JSON.globalNumberParser = myConversionFunc
 *
 * // Per-thread override
 * JSON.perThreadNumberParser = myConversionFunc
 * }}}
 *
 * @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
@deprecated("This object will be removed.", "2.11.0")
object JSON extends Parser {

  /**
   * This method converts ''raw'' results back into the original, deprecated
   * form.
   */
  private def unRaw (in : Any) : Any = in match {
    case JSONObject(obj) => obj.map({ case (k,v) => (k,unRaw(v))}).toList
    case JSONArray(list) => list.map(unRaw)
    case x => x
  }

  /**
   * Parse the given `JSON` string and return a list of elements. If the
   * string is a `JSON` object it will be a `JSONObject`. If it's a `JSON`
   * array it will be a `JSONArray`.
   *
   * @param input the given `JSON` string.
   * @return      an optional `JSONType` element.
   */
  def parseRaw(input : String) : Option[JSONType] =
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(result, _) => Some(result)
      case _ => None
    }

  /**
   * Parse the given `JSON` string and return either a `List[Any]`
   * if the `JSON` string specifies an `Array`, or a
   * `Map[String,Any]` if the `JSON` string specifies an object.
   *
   * @param input the given `JSON` string.
   * @return      an optional list or map.
   */
  def parseFull(input: String): Option[Any] =
    parseRaw(input) match {
      case Some(data) => Some(resolveType(data))
      case None => None
    }

  /**
   * A utility method to resolve a parsed `JSON` list into objects or
   * arrays. See the `parse` method for details.
   */
  def resolveType(input: Any): Any = input match {
    case JSONObject(data) => data.transform {
      case (k,v) => resolveType(v)
    }
    case JSONArray(data) => data.map(resolveType)
    case x => x
  }

  /**
   * The global (VM) default function for converting a string to a numeric value.
   */
  def globalNumberParser_=(f: NumericParser) { defaultNumberParser = f }
  def globalNumberParser : NumericParser = defaultNumberParser

  /**
   * Defines the function used to convert a numeric string literal into a
   * numeric format on a per-thread basis. Use `globalNumberParser` for a
   * global override.
   */
   def perThreadNumberParser_=(f : NumericParser) { numberParser.set(f) }
   def perThreadNumberParser : NumericParser = numberParser.get()
}
