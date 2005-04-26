/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml.dtd ;

import scala.collection.Map ;

abstract class Decl ;

abstract class MarkupDecl extends Decl ;

/** an element declaration
 */
case class ElemDecl(name: String, contentModel: ContentModel.RegExp, attList: AttListDecl) extends MarkupDecl {

  //def mixed  = ; // to do

  def setAttList(nAttList:AttListDecl) =
    ElemDecl(name, contentModel, nAttList);
} // ElemDecl

case class AttListDecl(name: String, attrs:List[AttrDecl]) extends MarkupDecl;

/** an attribute declaration. at this point, the tpe is a string. Future
 *  versions might provide a way to access the attribute types more
 *  directly.
 */
case class AttrDecl( name:String, tpe:String, default:DefaultDecl ) {
  final override def toString() = {
    val sb = new StringBuffer("AttrDecl(");
    sb.append('"');
    sb.append( name );
    sb.append('"');
    sb.append(',');
    sb.append('"');
    sb.append( tpe );
    sb.append('"');
    sb.append(',');
    sb.append(default.toString());
    sb.append(')');
    sb.toString();
  }
}

class EntityDecl extends MarkupDecl;
/** an entity declaration */

case class ParsedEntityDecl( name:String, entdef:EntityDef )
     extends EntityDecl;

case class ParameterEntityDecl(name: String, entdef: EntityDef)
     extends EntityDecl;

class EntityDef;

case class IntDef(value:String) extends EntityDef {
  private def validateValue(): Unit = {
    var tmp = value;
    var ix  = tmp.indexOf('%');
    while( ix != -1) {
      val iz = tmp.indexOf(';', ix);
      if(iz == -1 && iz == ix + 1)
        error("no % allowed in entity value, except for parameter-entity-references");
      else {
        val n = tmp.substring(ix, iz);

        if( !Utility.isName( n ))
          throw new IllegalArgumentException("ent must be an XML Name");

        tmp = tmp.substring(iz+1, tmp.length());
        ix  = tmp.indexOf('%');
      }
    }
  }
  validateValue();
}
case class ExtDef(extID:ExternalID) extends EntityDef;

/** an entity declaration */
case class UnparsedEntityDecl( name:String, extID:ExternalID, notation:String ) extends EntityDecl;

/** a notation declaration */
case class NotationDecl( name:String, extID:ExternalID ) extends MarkupDecl;

/** a parsed entity reference */
case class PEReference(ent:String) extends MarkupDecl {
  if( !Utility.isName( ent ))
    throw new IllegalArgumentException("ent must be an XML Name");

  final override def toString() = "%"+ent+";"
}


// default declarations for attributes

class DefaultDecl ;

case object REQUIRED extends DefaultDecl {
  final override def toString() = "REQUIRED";
}
case object IMPLIED extends DefaultDecl {
  final override def toString() = "IMPLIED";
}
case class DEFAULT(fixed:boolean, attValue:String) extends DefaultDecl {
  final override def toString() = {
    val sb = new StringBuffer("DEFAULT(");
    sb.append( fixed );
    sb.append(',');
    Utility.appendEscapedQuoted( attValue, sb );
    sb.append(')');
    sb.toString()
  }
}
