/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

/** an XML node for document type declaration
 *
 * @author Burak Emir
 * @param  target name of this DOCTYPE
 * @param  extID  None, or Some(external ID of this doctype)
 * @param  intSubset sequence of internal subset declarations
**/

case class DocType( name:String, extId:Option[ExternalID], intSubset:Seq[dtd.Decl]) extends Node {

  if( !Utility.isName( name ) )
    throw new IllegalArgumentException(target+" must be an XML Name");

  /** the constant "#DOCTYPE" */
  final def label    = "#DOCTYPE";

  /** always empty */
  final def attribute = Node.NoAttributes;

  /** always empty */
  final def child = Nil;

  /** hashcode for this processing instruction */
  final override def hashCode() = text.hashCode();

  /** returns "<!DOCTYPE + name + extID? + ("["+intSubSet+"]")? >" */
  final override def toString() = {
    val sb = new StringBuffer("<!DOCTYPE ");
    sb.append( dt );
    extID match {
      case Some(xid) => sb.append(' '), sb.append( xid.toString() )
      case _         =>
    }
    if( intSubset.length > 0 ) {
      sb.append('[');
      for( d <- intSubset ) {
        sb.append( d.toString() );
      }
      sb.append(']');
    }
    s.append('>');
  }
}
