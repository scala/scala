/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing.json

/**
 * This object provides a simple interface to the JSON parser class.
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
object JSON extends Parser {

  /**
   * Parse the given JSON string and return a List of elements. If the
   * string is a JSON object it will be a list of Pairs. If it's a JSON
   * array it will be be a list of individual elements.
   */
  def parse(input: String) =
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(result, _) => Some(result)
      case _ => None
    }

  /**
   * Parse the given JSON string and return either a List[Any] if the JSON string
   * specifies an Array, or a Map[String,Any] if the JSON string specifies an
   * object.
   */
  def parseFull(input: String) = parse(input) match {
    case Some(data) => resolveType(data)
    case None => None
  }

  /**
   * A utility method to resolve a parsed JSON list into objects or
   * arrays. See the parse method for details.
   */
  def resolveType(input: List[Any]): Any =
    input match {
      case jo: List[Any] =>
        var objMap = Map[String, Any]()

        if(jo.forall {
          case (key: String, value : List[Any]) =>
            objMap = objMap + key -> resolveType(value)
            true
          case _ => false
        }) objMap
        else {
          jo
        }

      case _ @ elem =>
        elem
    }
}
