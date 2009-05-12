/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing.json

/**
 * This object provides a simple interface to the JSON parser class. The default conversion
 * for numerics is into a double. If you wish to override this behavior at the global level,
 * you can set the globalNumberParser property to your own (String => Any) function. If you only
 * want to override at the per-thread level then you can set the perThreadNumberParser property to your
 * function. For example:
 *
 * <pre>
 * val myConversionFunc = {input : String => BigDecimal(input)}
 *
 * // Global override
 * JSON.globalNumberParser = myConversionFunc
 *
 * // Per-thread override
 * JSON.perThreadNumberParser = myConversionFunc
 * </pre>
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
object JSON extends Parser {

  /**
   * Parse the given JSON string and return a list of elements. If the
   * string is a JSON object it will be a list of pairs. If it's a JSON
   * array it will be be a list of individual elements.
   *
   * @param input the given JSON string.
   * @return      an optional list of of elements.
   */
  def parse(input: String): Option[List[Any]] =
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(result, _) => Some(result)
      case _ => None
    }

  /**
   * Parse the given JSON string and return either a <code>List[Any]</code>
   * if the JSON string specifies an <code>Array</code>, or a
   * <code>Map[String,Any]</code> if the JSON string specifies an object.
   *
   * @param input the given JSON string.
   * @return      an optional list or map.
   */
  def parseFull(input: String): Option[Any] =
    parse(input) match {
      case Some(data) => Some(resolveType(data))
      case None => None
    }

  /**
   * A utility method to resolve a parsed JSON list into objects or
   * arrays. See the parse method for details.
   */
  def resolveType(input: List[_]): Any = {
    var objMap = Map[String, Any]()

    if (input.forall {
      case (key: String, value: List[_]) =>
        objMap = objMap.+[Any](key -> resolveType(value))
        true
      case (key : String, value : Any) =>
        objMap += key -> value
        true
      case _ => false
    }) objMap
    else
      input
  }

  /**
   * The global (VM) default function for converting a string to a numeric value.
   */
  def globalNumberParser_=(f: NumericParser) { defaultNumberParser = f }
  def globalNumberParser : NumericParser = defaultNumberParser

  /**
   * Defines the function used to convert a numeric string literal into a numeric format on a per-thread
   * basis. Use globalNumberParser for a global override
   */
   def perThreadNumberParser_=(f : NumericParser) { numberParser.set(f) }
   def perThreadNumberParser : NumericParser = numberParser.get()
}
