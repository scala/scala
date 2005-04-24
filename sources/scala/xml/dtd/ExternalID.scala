/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml.dtd;

/** an ExternalIDs - either PublicID or SystemID
 *
 * @author Burak Emir
 * @param  target target name of this PI
 * @param  text   text contained in this node, may not contain "?>"
**/

class ExternalID ;

/** a system identifier
 *
 * @author Burak Emir
 * @param  systemLiteral the system identifier literal
**/

case class SystemID( systemLiteral:String ) extends ExternalID {

  if( !Parsing.checkSysID( systemLiteral ) )
    throw new IllegalArgumentException(
      "can't use both \" and ' in systemLiteral"
    );
  final override def toString() =
    Utility.systemLiteralToString( systemLiteral );
}


/** a public identifier
 *
 * @author Burak Emir
 * @param  publicLiteral the public identifier literal
 * @param  systemLiteral the system identifier literal
**/
case class PublicID( publicLiteral:String, systemLiteral:String ) extends ExternalID {

  if( !Parsing.checkPubID( publicLiteral ))
    throw new IllegalArgumentException(
      "publicLiteral must consist of PubidChars"
    );
  if( !Parsing.checkSysID( systemLiteral ) )
    throw new IllegalArgumentException(
      "can't use both \" and ' in systemLiteral"
    );

  /** the constant "#PI" */
  final def label    = "#PI";

  /** always empty */
  final def attribute = Node.NoAttributes;

  /** always empty */
  final def child = Nil;

  /** returns "PUBLIC "+publicLiteral+" SYSTEM "+systemLiteral */
  final override def toString() =
    Utility.publicLiteralToString( publicLiteral )
  + Utility.systemLiteralToString( systemLiteral );

}
