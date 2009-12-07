/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml
package pull

/** This class represents an XML event for pull parsing.
 *  Pull parsing means that during the traversal of the XML
 *  tree we are parsing, each "event" is returned to the caller
 *  and the traversal is suspended.
 */
trait XMLEvent

/** An element is encountered the first time */
case class EvElemStart(pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) extends XMLEvent

/** An element is encountered the last time */
case class EvElemEnd(pre: String, label: String) extends XMLEvent

/** A text node is encountered */
case class EvText(text: String) extends XMLEvent

/** An entity reference is encountered */
case class EvEntityRef(entity: String) extends XMLEvent

/** A processing instruction is encountered */
case class EvProcInstr(target: String, text: String) extends XMLEvent

/** A comment is encountered */
case class EvComment(text: String) extends XMLEvent
