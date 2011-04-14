/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml

/** <p>
 *    Use this class to match on (unprefixed) attribute values
 *  <p><pre>
 *  <b>val</b> hasName = <b>new</b> HasKeyValue("name")
 *  node <b>match</b> {
 *    <b>case</b> Node("foo", hasName(x), _*) => x // foo had attribute with key "name" and with value x
 *  }</pre>
 *
 *  @author Burak Emir
 */
@deprecated("Use UnprefixedAttribute's extractor", "2.8.0")
class HasKeyValue(key: String) {
  def unapplySeq(x: MetaData): Option[Seq[Node]] = x.get(key)
}
