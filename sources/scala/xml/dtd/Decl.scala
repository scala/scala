package scala.xml.dtd ;

import scala.collection.Map ;

abstract class Decl ;

abstract class MarkupDecl extends Decl ;

case class ElemDecl( name:String ,
                     contentModel:String ,
                     attribs:Map[String,AttrDecl] )
     extends MarkupDecl {

       def containsText = contentModel.indexOf("#PCDATA") != -1 ;
};

/* ignore default values 4 now */
case class AttrDecl( name:String, tpe:String, default:DefaultDecl ) extends MarkupDecl;

/* ignore default values 4 now */
case class EntityDecl( name:String, tpe:String ) extends MarkupDecl;

/* ignore default values 4 now */
case class NotationDecl( name:String, tpe:String ) extends MarkupDecl;

case class PEReference(ent:String) extends Decl {
  if( !Utility.isName( ent ))
    throw new IllegalArgumentException("ent must be an XML Name");

  final override def toString() = "%"+ent+";"
}


// default declarations for attributes

class DefaultDecl ;

case object REQUIRED, IMPLIED extends DefaultDecl;

case class DEFAULT(fixed:boolean, attValue:String) extends DefaultDecl;
