/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/** A hack to group XML nodes in one node for output.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
final case class Group(val nodes: Seq[Node]) extends Node {
  override def theSeq = nodes

  override def canEqual(other: Any) = other match {
    case x: Group => true
    case _        => false
  }

  override def strict_==(other: Equality) = other match {
    case Group(xs)  => nodes sameElements xs
    case _          => false
  }

  override protected def basisForHashCode = nodes

  /** Since Group is very much a hack it throws an exception if you
   *  try to do anything with it.
   */
  private def fail(msg: String) = throw new UnsupportedOperationException("class Group does not support method '%s'" format msg)

  def label                           = fail("label")
  override def attributes             = fail("attributes")
  override def namespace              = fail("namespace")
  override def child                  = fail("child")
  def buildString(sb: StringBuilder)  = fail("toString(StringBuilder)")
}
