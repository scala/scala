/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml

/** an XML node for processing instructions (PI)
 *
 * @author Burak Emir
 * @param  target     target name of this PI
 * @param  proctext   text contained in this node, may not contain "?>"
 */
case class ProcInstr(target: String, proctext: String) extends SpecialNode
{
  if (!Utility.isName(target))
    throw new IllegalArgumentException(target+" must be an XML Name")
  if (proctext contains "?>")
    throw new IllegalArgumentException(proctext+" may not contain \"?>\"")
  if (target.toLowerCase == "xml")
    throw new IllegalArgumentException(target+" is reserved")

  final override def doCollectNamespaces = false
  final override def doTransform         = false

  final def label   = "#PI"
  override def text = ""

  /** appends &quot;&lt;?&quot; target (&quot; &quot;+text)?+&quot;?&gt;&quot;
   *  to this stringbuffer.
   */
  override def buildString(sb: StringBuilder) =
    sb append "<?%s%s?>".format(target, (if (proctext == "") "" else " " + proctext))
}
