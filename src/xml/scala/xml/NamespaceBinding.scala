/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package xml

import Utility.sbToString

/** The class `NamespaceBinding` represents namespace bindings
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

  private def shadowRedefined(stop: NamespaceBinding): NamespaceBinding = {
    def prefixList(x: NamespaceBinding): List[String] =
      if ((x == null) || (x eq stop)) Nil
      else x.prefix :: prefixList(x.parent)
    def fromPrefixList(l: List[String]): NamespaceBinding = l match {
      case Nil => stop
      case x :: xs => new NamespaceBinding(x, this.getURI(x), fromPrefixList(xs))
    }
    val ps0 = prefixList(this).reverse
    val ps = ps0.distinct
    if (ps.size == ps0.size) this
    else fromPrefixList(ps)
  }

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

  def buildString(sb: StringBuilder, stop: NamespaceBinding) {
    shadowRedefined(stop).doBuildString(sb, stop)
  }

  private def doBuildString(sb: StringBuilder, stop: NamespaceBinding) {
    if ((this == null) || (this eq stop)) return    // contains?

    val s = " xmlns%s=\"%s\"".format(
      (if (prefix != null) ":" + prefix else ""),
      (if (uri != null) uri else "")
    )
    parent.doBuildString(sb append s, stop) // copy(ignore)
  }
}
