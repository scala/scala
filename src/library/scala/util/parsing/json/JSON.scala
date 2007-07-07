package scala.util.parsing.json

import scala.collection.mutable.HashMap

/** This object mainly shows how a JSON parser maybe instantiated
 *
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
        val objMap = new HashMap[String,Any]()

        jo.foreach {
          case (key,value) =>
            objMap.update(key,resolveType(value))
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
