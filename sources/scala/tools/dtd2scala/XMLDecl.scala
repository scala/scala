package scala.tools.dtd2scala ;

import java.util.Map ;

abstract class XMLDecl ;

case class ElemDecl( name:String ,
                     contentModel:String ,
                     attribs:Map ) extends XMLDecl; /*AttrDecl[]*/

// ignore default values 4 now
case class AttrDecl( name:String,
                     tpe:String ) extends XMLDecl;

