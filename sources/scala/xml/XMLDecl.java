package scala.xml ;

import java.util.Map ;

public class XMLDecl {

      case ElemDecl( String name,
                     String contentModel,
                     Map attribs); /*AttrDecl[]*/

      case AttrDecl( String name,
                     String type ); // ignore default values

}
