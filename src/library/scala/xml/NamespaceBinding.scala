/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml

import Utility.sbToString


/** The class <code>NamespaceBinding</code> represents namespace bindings
 *  and scopes. The binding for the default namespace is treated as a null
 *  prefix. the absent namespace is represented with the null uri. Neither
 *  prefix nor uri may be empty, which is not checked.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
@SerialVersionUID(0 - 2518644165573446725L)
case class NamespaceBinding(prefix: String, uri: String, parent: NamespaceBinding) extends AnyRef with Equality
{
  if (prefix == "")
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
  override def canEqual(other: Any) = other match {
    case _: NamespaceBinding  => true
    case _                    => false
  }
  override def strict_==(other: Equality) = other match {
    case x: NamespaceBinding  => (prefix == x.prefix) && (uri == x.uri) && (parent == x.parent)
    case _                    => false
  }
  def basisForHashCode: Seq[Any] = List(prefix, uri, parent)

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
