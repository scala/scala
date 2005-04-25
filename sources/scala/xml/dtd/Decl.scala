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

case class ElemDecl( name:String ,
                     contentModel:String ,
                     attribs:Map[String,AttrDecl] )
     extends MarkupDecl {

       final val parsedContentModel:ContentModel.RegExp = {
         try {
           ContentModel.parse( contentModel );
         } catch {
           case _:Error =>
             Console.println( "error parsing declaration of " + name );
             Console.println( "content model was:\n" + contentModel );
             null
         }
       }

       def containsText = contentModel.indexOf("#PCDATA") != -1 ;
};

/** an attribute declaration */
case class AttrDecl( name:String, tpe:String, default:DefaultDecl ) extends MarkupDecl {
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

/** an entity declaration */
case class EntityDecl( name:String, tpe:String ) extends MarkupDecl;

/** a notation declaration */
case class NotationDecl( name:String, tpe:String ) extends MarkupDecl;

/** a parsed entity reference */
case class PEReference(ent:String) extends Decl {
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
