package scala.tools.dtd2scala ;

import scala.xml.dtd._ ;
import scala.collection.mutable.HashMap ;

class MyElemDecl( name:String ,
                 contentModel:String ,
                 theAttribs:HashMap[String,AttrDecl] ) extends ElemDecl( name, contentModel, theAttribs ) {
                   def myAttribs = theAttribs;
};
