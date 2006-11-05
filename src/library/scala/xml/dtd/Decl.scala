/*                     __                                               *\
 **     ________ ___   / /  ___     Scala API                            **
 **    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
 **  __\ \/ /__/ __ |/ /__/ __ |                                         **
 ** /____/\___/_/ |_/____/_/ | |                                         **
 **                          |/                                          **
 \*                                                                      */

// $Id$


package scala.xml.dtd;


import compat.StringBuilder

abstract class Decl;

abstract class MarkupDecl extends Decl {
  def toString(sb: StringBuilder): StringBuilder;
}

/** an element declaration
 */
case class ElemDecl(name: String, contentModel: ContentModel) extends MarkupDecl with DtdTypeSymbol {

  //def mixed  = ; // to do

  override def toString(sb: StringBuilder): StringBuilder = {
    sb
    .append("<!ELEMENT ")
    .append(name)
    .append(' ');

    ContentModel.toString(contentModel, sb);
    sb.append('>');
  }

} // ElemDecl

case class AttListDecl(name: String, attrs:List[AttrDecl]) extends MarkupDecl with DtdTypeSymbol {

  override def toString(sb: StringBuilder): StringBuilder = {
    sb
    .append("<!ATTLIST ")
    .append(name)
    .append('\n')
    .append(attrs.mkString("","\n",">"));
  }
}

/** an attribute declaration. at this point, the tpe is a string. Future
 *  versions might provide a way to access the attribute types more
 *  directly.
 */
case class AttrDecl( name:String, tpe:String, default:DefaultDecl ) {

  override def toString(): String =
    toString(new StringBuilder()).toString();

 def toString(sb: StringBuilder): StringBuilder = {
    sb.append("  ").append( name ).append(' ').append( tpe ).append(' ');
    default.toString(sb)
  }

}

/** an entity declaration */
abstract class EntityDecl extends MarkupDecl;

/** a parsed general entity declaration */
case class ParsedEntityDecl( name:String, entdef:EntityDef ) extends EntityDecl {

  override def toString(sb: StringBuilder): StringBuilder = {
    sb.append("<!ENTITY ").append( name ).append(' ');
    entdef.toString(sb).append('>');
  }
}

/** a parameter entity declaration */
case class ParameterEntityDecl(name: String, entdef: EntityDef) extends EntityDecl {
  override def toString(sb: StringBuilder): StringBuilder = {
    sb.append("<!ENTITY % ").append( name ).append(' ');
    entdef.toString(sb).append('>');
  }
}

/** an unparsed entity declaration */
case class UnparsedEntityDecl( name:String, extID:ExternalID, notation:String ) extends EntityDecl {
  override def toString(sb: StringBuilder): StringBuilder = {
    sb.append("<!ENTITY ").append( name ).append(' ');
    extID.toString(sb).append(" NDATA ").append(notation).append('>');
  }
}
/** a notation declaration */
case class NotationDecl( name:String, extID:ExternalID ) extends MarkupDecl {
  override def toString(sb: StringBuilder): StringBuilder = {
    sb.append("<!NOTATION ").append( name ).append(' ');
    extID.toString(sb);
  }
}

abstract class EntityDef {
  def toString(sb: StringBuilder): StringBuilder;
}

case class IntDef(value:String) extends EntityDef {
  private def validateValue(): Unit = {
    var tmp = value;
    var ix  = tmp.indexOf('%');
    while( ix != -1) {
      val iz = tmp.indexOf(';', ix);
      if(iz == -1 && iz == ix + 1)
        throw new IllegalArgumentException("no % allowed in entity value, except for parameter-entity-references");
      else {
        val n = tmp.substring(ix, iz);

        if( !Utility.isName( n ))
          throw new IllegalArgumentException("interal entity def: \""+n+"\" must be an XML Name");

        tmp = tmp.substring(iz+1, tmp.length());
        ix  = tmp.indexOf('%');
      }
    }
  }
  validateValue();

  override def toString(sb: StringBuilder): StringBuilder =
    Utility.appendQuoted(value, sb);

}

case class ExtDef(extID:ExternalID) extends EntityDef {
  override def toString(sb: StringBuilder): StringBuilder =
    extID.toString(sb);
}



/** a parsed entity reference */
case class PEReference(ent:String) extends MarkupDecl {
  if( !Utility.isName( ent ))
    throw new IllegalArgumentException("ent must be an XML Name");

  override def toString(sb: StringBuilder): StringBuilder =
    sb.append('%').append(ent).append(';');
}


// default declarations for attributes

abstract class DefaultDecl {
  override def toString(): String;
  def toString(sb: StringBuilder): StringBuilder;
}

case object REQUIRED extends DefaultDecl {
  override def toString(): String = "#REQUIRED";
  override def toString(sb:StringBuilder) = sb.append("#REQUIRED");
}

case object IMPLIED extends DefaultDecl {
  override def toString(): String = "#IMPLIED";
  override def toString(sb:StringBuilder) = sb.append("#IMPLIED");
}

case class DEFAULT(fixed: Boolean, attValue:String) extends DefaultDecl {
  override def toString(): String =
    toString(new StringBuilder()).toString();

  override def toString(sb:StringBuilder): StringBuilder = {
    if(fixed)
      sb.append("#FIXED ");
    Utility.appendEscapedQuoted( attValue, sb );
  }
}
