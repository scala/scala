package scala.xml.dtd ;

import scala.collection.Map ;

abstract class Decl ;

abstract class MarkupDecl extends Decl ;

case class ElemDecl( name:String ,
                     contentModel:String ,
                     attribs:Map[String,AttrDecl] )
     extends MarkupDecl {

       final val parsedContentModel:RegExp = {
         try {
           RegExp.parse( contentModel );
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
case class AttrDecl( name:String, tpe:String, default:DefaultDecl ) extends MarkupDecl;

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

case object REQUIRED, IMPLIED extends DefaultDecl;

case class DEFAULT(fixed:boolean, attValue:String) extends DefaultDecl;
