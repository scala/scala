/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.pull

/** This class represents an XML event for pull parsing.
 */
trait XMLEvent

case class ElemStart(pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) extends XMLEvent

case class ElemEnd(pre: String, label: String) extends XMLEvent
