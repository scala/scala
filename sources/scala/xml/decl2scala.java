package scala.xml ;

import org.xml.sax.SAXParseException;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.Attributes;
import org.xml.sax.ext.DeclHandler;

import java.util.Map ;
import java.util.HashMap ;
import java.util.TreeMap ;
import java.util.Iterator ;

import XMLDecl.* ;

/** 2 do :
    - use DTD Handler to get the name of the DTD, use this as module name
    - handle modes of attributes (add #REQUIRED ones, fill in default of #IMPLIED)
    - allow package prefix to be given !
    - allow output directory to be given !
 */

public class dtd2scala
      extends DefaultHandler
      implements DeclHandler
{

      static final String
            DEFAULT_PARSER_NAME = "org.apache.xerces.parsers.SAXParser" ;

      public Map elemMap;   // elementName -> elementDecl

          public dtd2scala() {
                elemMap = new HashMap();
          }

          // DTDHandler methods

          /** Element declaration. */
          public void elementDecl(String name, String contentModel)
                throws SAXException {

                ElemDecl decl = (ElemDecl) elemMap.get( name );

                if( decl == null ) {
                      elemMap.put( name, decl = new ElemDecl( name,
                                                                 contentModel,
                                                                 new HashMap() ));
                } else {
                      decl.contentModel = contentModel;
                }
                /*
                System.out.print("element:");

                System.out.print("[name:" + name);

                System.out.print(" contentModel:" + contentModel);

                System.out.println("]");
                */
          } // elementDecl(String,String)

          /** Attribute declaration. */
          public void attributeDecl(String elementName, String attributeName,
                                    String type, String valueDefault,
                                    String value) throws SAXException {
                Map attribs;

                ElemDecl decl = (ElemDecl) elemMap.get( elementName );

                if( decl == null ) {
                      attribs = new TreeMap();
                      elemMap.put( elementName,
                                   decl = new ElemDecl( elementName,
                                                           null,
                                                           attribs ));
                } else {
                      attribs = decl.attribs;
                }

                attribs.put( attributeName, new AttrDecl(attributeName,
                                                         type ));

                /*
                System.out.print("attributes[");
                System.out.print("elementName=" + elementName+",");

                System.out.print("attributeName=" + attributeName);

                System.out.println("]");
                */
          } // attributeDecl(String,String,String,String,String)


      /** Internal entity declaration. */
      public void internalEntityDecl(String name, String text)
            throws SAXException {
            // ignore
      }

      /** External entity declaration. */
      public void externalEntityDecl(String name,
                                     String publicId, String systemId)
            throws SAXException {
            // ignore
      }

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


      public void print2( String moduleName ) {

            DeclToScala translate = new DeclToScala( moduleName );

            translate.toScala( elemMap );

      }
      // main

      public static void main(String[] argv) throws Exception {


            if( argv.length != 2 ) {
                  System.out.println("argument ?");
                  System.exit( 0 );
            }

            String sysID = argv[ 0 ];
            dtd2scala myH = new dtd2scala();

            XMLReader parser =
                  XMLReaderFactory.createXMLReader( DEFAULT_PARSER_NAME );

            try {
                  parser.setProperty("http://xml.org/sax/properties/declaration-handler", myH);
            }
            catch (SAXException e) {
                  e.printStackTrace(System.err);
            }

            try {
                  parser.parse( sysID );
            }
            catch (SAXParseException e) {

                  System.err.println("SaxParseEx");
                  e.printStackTrace( System.err );

            }
            catch (Exception e) {
                  System.err.println("error: Parse error occurred - "+e.getMessage());
                  if (e instanceof SAXException) {
                        e = ((SAXException)e).getException();
                  }
                  e.printStackTrace(System.err);
            }

            //myH.print();

            myH.print2( argv[ 1 ] ); // todo:replace by root element
      } // main


} // class MyDTDHandler
