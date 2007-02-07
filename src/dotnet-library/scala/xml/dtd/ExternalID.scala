/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.dtd


import compat.StringBuilder

/** an ExternalIDs - either PublicID or SystemID
 *
 * @author Burak Emir
 * @param  target target name of this PI
 * @param  text   text contained in this node, may not contain "?>"
**/

abstract class ExternalID  {

  /** returns "PUBLIC "+publicLiteral+" SYSTEM "+systemLiteral */
  override def toString(): String

  /** returns "PUBLIC "+publicLiteral+" SYSTEM "+systemLiteral */
   def toString(sb: StringBuilder): StringBuilder

  def systemId: String

}

/** a system identifier
 *
 * @author Burak Emir
 * @param  systemLiteral the system identifier literal
**/

case class SystemID( systemId:String ) extends ExternalID with parsing.TokenTests{

  if( !checkSysID( systemId ) )
    throw new IllegalArgumentException(
      "can't use both \" and ' in systemLiteral"
    )
  /** returns " SYSTEM "+systemLiteral */
  override def toString() =
    Utility.systemLiteralToString( systemId )

  override def toString(sb: StringBuilder): StringBuilder =
    Utility.systemLiteralToString( sb, systemId )
}


/** a public identifier
 *
 * @author Burak Emir
 * @param  publicLiteral the public identifier literal
 * @param  systemLiteral (can be null for notation pubIDs) the system identifier literal
**/
case class PublicID( publicId:String, systemId:String ) extends ExternalID with parsing.TokenTests{

  if( !checkPubID( publicId ))
    throw new IllegalArgumentException(
      "publicId must consist of PubidChars"
    )
  if( (systemId ne null) && !checkSysID( systemId ) )
    throw new IllegalArgumentException(
      "can't use both \" and ' in systemId"
    )

  /** the constant "#PI" */
  def label    = "#PI"

  /** always empty */
  def attribute = Node.NoAttributes

  /** always empty */
  def child = Nil

  /** appends "PUBLIC "+publicId+" SYSTEM "+systemId to argument */
  override def toString(sb: StringBuilder): StringBuilder = {
    Utility.publicLiteralToString( sb, publicId ).append(' ')
    if(systemId ne null)
      Utility.systemLiteralToString( sb, systemId )
    else
      sb
  }
}
