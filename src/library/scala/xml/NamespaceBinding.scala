/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import Predef._
import Utility.sbToString
import collection.mutable.StringBuilder

/** The class <code>NamespaceBinding</code> represents namespace bindings
 *  and scopes. The binding for the default namespace is treated as a null
 *  prefix. the absent namespace is represented with the null uri. Neither
 *  prefix nor uri may be empty, which is not checked.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
@SerialVersionUID(0 - 2518644165573446725L)
case class NamespaceBinding(prefix: String, uri: String, parent: NamespaceBinding) extends AnyRef
{
  if (prefix != null && prefix.isEmpty)
    throw new IllegalArgumentException("zero length prefix not allowed")

  def getURI(_prefix: String): String =
    if (prefix == _prefix) uri else parent getURI _prefix

  /** Returns some prefix that is mapped to the URI.
   *
   * @param _uri the input URI
   * @return the prefix that is mapped to the input URI, or null
   * if no prefix is mapped to the URI.
   */
  def getPrefix(_uri: String): String =
    if (_uri == uri) prefix else parent getPrefix _uri

  override def toString(): String = sbToString(buildString(_, TopScope))

  def buildString(stop: NamespaceBinding): String = sbToString(buildString(_, stop))
  def buildString(sb: StringBuilder, stop: NamespaceBinding): Unit = {
    if (this eq stop) return    // contains?

    val s = " xmlns%s=\"%s\"".format(
      (if (prefix != null) ":" + prefix else ""),
      (if (uri != null) uri else "")
    )
    parent.buildString(sb append s, stop) // copy(ignore)
  }
}
