/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

/** an XML node for processing instructions (PI)
 *
 * @author Burak Emir
 * @param  target target name of this PI
 * @param  text   text contained in this node, may not contain "?>"
**/

case class ProcInstr( target:String, text:String ) extends Node {

  val z:Seq[Char] = target; z match {
    case Seq('X'|'x','M'|'m','L'|'l') =>
      throw new IllegalArgumentException(target+" is reserved");
    case _ =>
  }
  if( !Utility.isName( target ) )
    throw new IllegalArgumentException(target+" must be an XML Name");
  else if( text.indexOf("?>" ) != -1 )
    throw new IllegalArgumentException(text+" may not contain \"?>\"");


  /** the constant "#PI" */
  final def label    = "#PI";

  /** always empty */
  final def attribute = Node.NoAttributes;

  /** always empty */
  final def child = Nil;

  /** hashcode for this PI */
  override def hashCode() = target.hashCode() * 7 + text.hashCode();

  /** returns "<?"+text+"?>" */
  final override def toString() = "<?"+text+"?>";

}
