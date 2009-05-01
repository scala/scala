/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

/** an XML node for processing instructions (PI)
 *
 * @author Burak Emir
 * @param  target target name of this PI
 * @param  text   text contained in this node, may not contain "?>"
 */
case class ProcInstr(target:String, proctext:String) extends SpecialNode {

  if (!Utility.isName(target))
    throw new IllegalArgumentException(target+" must be an XML Name")
  else if (text.indexOf("?>") != -1)
    throw new IllegalArgumentException(proctext+" may not contain \"?>\"")

  final override def typeTag$: Int = -2

  (target: Seq[Char]) match {
    case Seq('X'|'x','M'|'m','L'|'l') =>
      throw new IllegalArgumentException(target+" is reserved")
    case _ =>
  }

  /** structural equality */
  override def equals(x: Any): Boolean = x match {
    case ProcInstr(x, y) => x.equals(target) && y.equals(proctext)
    case _ => false
  }

  /** the constant "#PI" */
  final def label = "#PI"

  /** hashcode for this PI */
  override def hashCode() = target.hashCode() * 7 + proctext.hashCode()


  override def text = ""

  /** appends &quot;&lt;?&quot; target (&quot; &quot;+text)?+&quot;?&gt;&quot;
   *  to this stringbuffer.
   */
  override def buildString(sb: StringBuilder) = {
    sb
    .append("<?")
    .append(target);
    if (proctext.length() > 0) {
      sb
      .append(' ')
      .append(proctext);
    }
    sb.append("?>")
  }
}
