package scala.tools.dtd2scala ;

import scala.collection.mutable.HashMap ;

abstract class XMLDecl ;

case class ElemDecl( name:String ,
                     contentModel:String ,
                     attribs:HashMap[String,AttrDecl] )
     extends XMLDecl; /*AttrDecl[]*/

// ignore default values 4 now
case class AttrDecl( name:String,
                     tpe:String ) extends XMLDecl;

