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

case class ProcInstr( target:String, text:Option[String] ) extends SpecialNode {

  final override def typeTag$:Int = -2;

  val z:Seq[Char] = target; z match {
    case Seq('X'|'x','M'|'m','L'|'l') =>
      throw new IllegalArgumentException(target+" is reserved");
    case _ =>
  }
  if( !Parsing.isName( target ) )
    throw new IllegalArgumentException(target+" must be an XML Name");
  else text match {
    case Some(txt) => if( txt.indexOf("?>" ) != -1 )
      throw new IllegalArgumentException(txt+" may not contain \"?>\"");
    case _ =>
  }

  final override def equals(x:Any) = x match {
    case x @ ProcInstr( t2, s2 ) => target.equals( t2 ) && text.equals( s2 );
    case _ => false;
  }

  /** the constant "#PI" */
  final def label    = "#PI";

  /** hashcode for this PI */
  override def hashCode() = target.hashCode() * 7 + text.hashCode();

  /** returns &quot;&lt;?&quot;+target+(&quot; &quot;+text)?+&quot;?&gt;&quot; */
  final override def toString() = {
    val sb = new StringBuffer("<?");
    sb.append(target);
    text match {
      case Some(txt) =>
        sb.append(' ');
        sb.append(txt);
      case _ =>
    };
    sb.append("?>");
    sb.toString()
  }
}
