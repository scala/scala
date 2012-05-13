/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/** The class `EntityRef` implements an XML node for entity references.
 *
 * @author  Burak Emir
 * @version 1.0
 * @param   entityName the name of the entity reference, for example `amp`.
 */
case class EntityRef(entityName: String) extends SpecialNode {
  final override def doCollectNamespaces = false
  final override def doTransform         = false
  def label = "#ENTITY"

  override def text = entityName match {
    case "lt"   => "<"
    case "gt"   => ">"
    case "amp"  => "&"
    case "apos" => "'"
    case "quot" => "\""
    case _      => Utility.sbToString(buildString)
  }

  /** Appends `"&amp; entityName;"` to this string buffer.
   *
   *  @param  sb the string buffer.
   *  @return the modified string buffer `sb`.
   */
  override def buildString(sb: StringBuilder) =
    sb.append("&").append(entityName).append(";")

}
