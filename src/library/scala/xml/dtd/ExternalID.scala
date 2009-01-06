/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.dtd


/** an ExternalIDs - either PublicID or SystemID
 *
 *  @author Burak Emir
 */
abstract class ExternalID  {

  /** returns "PUBLIC "+publicLiteral+" SYSTEM "+systemLiteral */
  override def toString(): String

  /** returns "PUBLIC "+publicLiteral+" SYSTEM "+systemLiteral */
  def toString(sb: StringBuilder): StringBuilder

  def systemId: String

}

/** a system identifier
 *
 *  @author Burak Emir
 *  @param  systemLiteral the system identifier literal
 */
case class SystemID(systemId: String) extends ExternalID with parsing.TokenTests {

  if( !checkSysID(systemId) )
    throw new IllegalArgumentException(
      "can't use both \" and ' in systemLiteral"
    )
  /** returns " SYSTEM "+systemLiteral */
  override def toString() =
    Utility.systemLiteralToString(systemId)

  override def toString(sb: StringBuilder): StringBuilder =
    Utility.systemLiteralToString(sb, systemId)
}


/** a public identifier (see http://www.w3.org/QA/2002/04/valid-dtd-list.html).
 *
 *  @author Burak Emir
 *  @param  publicLiteral the public identifier literal
 *  @param  systemLiteral (can be null for notation pubIDs) the system identifier literal
 */
case class PublicID(publicId: String, systemId: String)
extends ExternalID with parsing.TokenTests {

  if (!checkPubID(publicId))
    throw new IllegalArgumentException(
      "publicId must consist of PubidChars"
    )
  if ((systemId ne null) && !checkSysID(systemId))
    throw new IllegalArgumentException(
      "can't use both \" and ' in systemId"
    )

  /** the constant "#PI" */
  def label = "#PI"

  /** always empty */
  def attribute = Node.NoAttributes

  /** always empty */
  def child = Nil

  /** returns " PUBLIC "+publicId+" "+systemId */
  override def toString() =
    toString(new StringBuilder()).toString()

  /** appends "PUBLIC "+publicId+" "+systemId to argument */
  override def toString(sb: StringBuilder): StringBuilder = {
    Utility.publicLiteralToString(sb, publicId)
    if (systemId ne null) {
      sb.append(' ')
      Utility.appendQuoted(systemId, sb)
    }
    sb
  }
}
