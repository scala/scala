package scala.tools.dtd2scala ;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.ext.DeclHandler;

import scala.collection.mutable.HashMap ;
// 2 do :  - handle modes of attributes ( #REQUIRED,  defaults for #IMPLIED )

/** SAX handler, reacts to events during declaration parsing */
class MainHandler extends DefaultHandler with DeclHandler {

  var elemMap:HashMap[String,ElemDecl] = new HashMap[String,ElemDecl];

  //                                        zzz    DTDHandler methods    zzz

  /** encountered element declaration */
  def elementDecl( name:String, contentModel:String ) = {
    elemMap.get( name ).match {
      case Some(decl) => // was added because of ATTLIST decl before
        elemMap.update( name, ElemDecl( decl.name,
                                        contentModel,
                                        decl.attribs ) )
      case None =>
        elemMap.update( name, ElemDecl( name,
                                        contentModel,
                                        new HashMap[String,AttrDecl]() ));
    }
  } // elementDecl(String,String)

  /** encountered attribute declaration. */
  def attributeDecl(elementName:String,
                    attributeName:String,
                    tpe:String,
                    valueDefault:String,
                    value:String ) = {

     val attribs = elemMap.get( elementName ).match {
	 case None => { val amap = new HashMap[String,AttrDecl];
		        elemMap.update( elementName,
					ElemDecl( elementName,
                                        null:String,
                                        amap ));
                        amap; }
       	 case Some(decl) => decl.attribs;
     };
     attribs.update( attributeName, AttrDecl(attributeName, tpe ));
  } // attributeDecl(String,String,String,String,String)


  /** Internal entity declaration. */
  def internalEntityDecl( name:String, text:String ) = {/*ignored*/}

  /** External entity declaration. */
  def externalEntityDecl(name:String,pubId:String,sysId:String)= {/*ignored*/}

} // class MyDTDHandler

