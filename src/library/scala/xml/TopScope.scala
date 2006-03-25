/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml;

/** top level namespace scope. only contains the predefined binding
 *  for the &quot;xml&quot; prefix which is bound to
 *  &quot;http://www.w3.org/XML/1998/namespace&quot;
 */
case object TopScope extends NamespaceBinding(null, null, null) {

  override def getURI(_prefix: String) =
    if(_prefix == "xml")
      return "http://www.w3.org/XML/1998/namespace";
    else
      return null;

  override def getPrefix(_uri: String) = null;
    if(_uri == "http://www.w3.org/XML/1998/namespace")
	  return "xml";
    else
      return null;

  override def toString() = "";

  override def toString(stop: NamespaceBinding) = "";

  override def toString(sb: StringBuffer, ignore: NamespaceBinding) = {};

}
