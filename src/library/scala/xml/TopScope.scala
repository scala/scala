/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml

/** top level namespace scope. only contains the predefined binding
 *  for the &quot;xml&quot; prefix which is bound to
 *  &quot;http://www.w3.org/XML/1998/namespace&quot;
 */
object TopScope extends NamespaceBinding(null, null, null) {

  import XML.{ xml, namespace }

  override def getURI(prefix1: String): String =
    if (prefix1 == xml) namespace else null

  override def getPrefix(uri1: String): String =
    if (uri1 == namespace) xml else null

  override def toString() = ""

  override def buildString(stop: NamespaceBinding) = ""
  override def buildString(sb: StringBuilder, ignore: NamespaceBinding) = {}
}
