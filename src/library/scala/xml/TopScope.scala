/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$
package scala.xml;

import Predef._

/** top level namespace scope. only contains the predefined binding
 *  for the &quot;xml&quot; prefix which is bound to
 *  &quot;http://www.w3.org/XML/1998/namespace&quot;
 */
case object TopScope extends NamespaceBinding(null, null, null) {

  override def getURI(prefix1: String): String =
    if(prefix1 == XML.prefix)
      XML.namespace;
    else
      null;

  override def getPrefix(uri1: String): String =
    if(uri1 == XML.namespace)
	  XML.prefix;
    else
      null;

  override def toString() = "";

  override def toString(stop: NamespaceBinding) = "";

  override def toString(sb: StringBuffer, ignore: NamespaceBinding) = {};

}
