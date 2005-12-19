/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml;

case object TopScope extends NamespaceBinding(null, null, null) {

  /*
  override def contains(pre:String) = false;
  */
  override def getURI(_prefix: String) = null;

  override def getPrefix(_uri: String) = null;

  override def toString() = "";

  override def toString(stop: NamespaceBinding) = "";

  override def toString(sb: StringBuffer, ignore: NamespaceBinding) = {};

}
