/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing.json

import scala.collection.mutable.HashMap

/** This object mainly shows how a JSON parser may be instantiated.
 *
 *  @author Derek Chen-Becker <java@chen-becker.org>
 */
object JSON extends Parser {

  def parse(input: String) =
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(result, _) => Some(result)
      case _ => None
    }

  def parseFull(input: String) = parse(input) match {
    case Some(data) => resolveType(data)
    case None => None
  }

  def resolveType(input: Any): Any =
    input match {
      case jo: List[(String,Any)] =>
        /*println("Resolving object")*/
        val objMap = new HashMap[String, Any]()

        jo.foreach {
          case (key, value) =>
            objMap.update(key, resolveType(value))
        }

          objMap

      case ja: List[Any] =>
        /*println("Resolving array"); */
        ja.toArray
      case _ @ elem =>
        /*println("Resolving element"); */
        elem
    }
}
