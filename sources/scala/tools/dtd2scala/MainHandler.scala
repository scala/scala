package scala.tools.dtd2scala ;

//import org.xml.sax.SAXParseException;
//import org.xml.sax.SAXException;
//import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Attributes;
import org.xml.sax.ext.DeclHandler;

/*
import java.util.Map ;
import java.util.HashMap ;
import java.util.TreeMap ;
import java.util.Iterator ;
*/
//import scala.collection.Map ;
import scala.collection.mutable.HashMap ;
/** 2 do :
    - handle modes of attributes (add #REQUIRED ones, fill in default of #IMPLIED)
    - allow package prefix to be given !
    - allow output directory to be given !
*/

class MainHandler extends DefaultHandler with DeclHandler {

  var elemMap:HashMap[String,ElemDecl] = new HashMap[String,ElemDecl];

  // DTDHandler methods

  /** encountered element declaration
   */
  def elementDecl( name:String, contentModel:String ):Unit
  /* throws SAXException */ = {

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

  /** encountered attribute declaration.
   */
  def attributeDecl(elementName:String,
                    attributeName:String,
                    tpe:String,
                    valueDefault:String,
                    value:String ) /*throws SAXException*/ = {

     var attribs:HashMap[String,AttrDecl] =
       elemMap.get( elementName ).match {
	 case None =>
           val amap = new HashMap[String,AttrDecl];
           elemMap.update( elementName,
                           ElemDecl( elementName,
                                     null:String,
                                     amap ));
           amap;

	 case Some(decl) =>
           decl.attribs;
     };
     attribs.update( attributeName, AttrDecl(attributeName, tpe ));
  } // attributeDecl(String,String,String,String,String)


  /** Internal entity declaration.
  */
  def internalEntityDecl( name:String, text:String ):Unit
        /*throws SAXException*/ = {
        // ignore
    }

    /** External entity declaration.
     */
  def externalEntityDecl( name:String,
                          publicId:String ,
                          systemId:String ):Unit
        /*throws SAXException*/ = {
        // ignore
    }

    /*

    // for debugging

    public void print() {
    for(Iterator it = elemMap.keySet().iterator(); it.hasNext(); ) {
    ElemDecl decl = (ElemDecl) elemMap.get( it.next() );
    System.out.print(decl.name);
    AttrDecl adecl = null;
    System.out.print("{");
    for(Iterator it2 = decl.attribs.keySet().iterator();
                it2.hasNext(); ) {
                if(adecl != null)
                    System.out.print(",");
                adecl = (AttrDecl) decl.attribs.get( it2.next() );
                System.out.print(adecl.name+":"+adecl.type);
            }
            System.out.println("}");
        }


    }
    */

} // class MyDTDHandler

