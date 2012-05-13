/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml
package dtd

/** an ExternalIDs - either PublicID or SystemID
 *
 *  @author Burak Emir
 */
abstract class ExternalID extends parsing.TokenTests
{
  def quoted(s: String) = {
    val c = if (s contains '"') '\'' else '"'
    c + s + c
  }

  // public != null: PUBLIC " " publicLiteral " " [systemLiteral]
  // public == null: SYSTEM " " systemLiteral
  override def toString(): String = {
    lazy val quotedSystemLiteral = quoted(systemId)
    lazy val quotedPublicLiteral = quoted(publicId)

    if (publicId == null) "SYSTEM " + quotedSystemLiteral
    else "PUBLIC " + quotedPublicLiteral +
      (if (systemId == null) "" else " " + quotedSystemLiteral)
  }
  def buildString(sb: StringBuilder): StringBuilder =
    sb.append(this.toString())

  def systemId: String
  def publicId: String
}

/** a system identifier
 *
 *  @author Burak Emir
 *  @param  systemId the system identifier literal
 */
case class SystemID(systemId: String) extends ExternalID {
  val publicId = null

  if (!checkSysID(systemId))
    throw new IllegalArgumentException("can't use both \" and ' in systemId")
}


/** a public identifier (see http://www.w3.org/QA/2002/04/valid-dtd-list.html).
 *
 *  @author Burak Emir
 *  @param  publicId the public identifier literal
 *  @param  systemId (can be null for notation pubIDs) the system identifier literal
 */
case class PublicID(publicId: String, systemId: String) extends ExternalID {
  if (!checkPubID(publicId))
    throw new IllegalArgumentException("publicId must consist of PubidChars")

  if (systemId != null && !checkSysID(systemId))
    throw new IllegalArgumentException("can't use both \" and ' in systemId")

  /** the constant "#PI" */
  def label = "#PI"

  /** always empty */
  def attribute = Node.NoAttributes

  /** always empty */
  def child = Nil
}
