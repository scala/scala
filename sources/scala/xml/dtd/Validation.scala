package scala.xml.dtd ;

import scala.collection.mutable;
import scala.collection.Set;
import scala.collection.Map;

class ValidationException( e:String ) extends Exception( e );

object ValidationException {
  def fromFixedAttribute( k: String, value: String, actual: String ) =
    new ValidationException("value of attribute " + k + " FIXED to \""+value+"\", but document tries \""+actual+"\"");

  def fromUndefinedAttribute( key:String ) =
    new ValidationException("undefined attribute " + key );

  def fromMissingAttribute( allKeys:Set[String] ) = {
    new ValidationException("missing value for REQUIRED attribute"+
                            { if( allKeys.size > 1 ) "s" else "" }+
                            allKeys );
  }
}

/** only CDATA attributes, ignores attributes that have different namespace
* @param: attrSpec
*/
class AttributeValidator( namespace$$:String, attrSpec1: Seq[dtd.AttrDecl]) {
  final val namespace = namespace$$.intern();
  final val attrSpec = new mutable.HashMap[String,AttrDecl]();
  var attribsTemplate:List[Attribute] = Nil;
  for( val adecl <- attrSpec1 ) {
    attrSpec.update( adecl.name, adecl )
  }

  var req = 0;
  val map = new mutable.HashMap[String,String]();

  // populate with special "<R" and default
  for( val key <- attrSpec.keys ) {
    attrSpec( key ).default match {
      case dtd.REQUIRED =>
        map.update( key, "<R" );
      req = req + 1;
      case dtd.DEFAULT( _, attValue ) =>
        map.update( key, attValue );
        attribsTemplate =
          add( attribsTemplate, Attribute(namespace, key, attValue));
      case dtd.IMPLIED  =>
    }
  }

  def add(as:List[Attribute], x:Attribute):List[Attribute] = {
    if( x.namespace.length() == 0 )
      scala.xml.Attribute( namespace, x.key, x.value )::as;
    else
      x::as;
  };

  def validate( attributes:AttributeSeq ):AttributeSeq = {
    var actualReq = 0;
    var attribs: List[Attribute] = Nil;
    for( val b <- attributes ) {
      if( b.key == "xmlns" ) {} // this should not get even here
      else if( b.namespace != namespace )
        attribs = add( attribs, b);
      else attrSpec.get( b.key ) match {
        case Some( AttrDecl( key, tpe, df )) => df match {
          case DEFAULT( true, attValue ) =>
            if( b.value != attValue )
              ValidationException.fromFixedAttribute( key, attValue, b.value );
            else
              add( attribs, b )
          case REQUIRED =>
            actualReq = actualReq + 1;
          attribs = add( attribs, b )
          case _ =>
            attribs = add( attribs, b )
        }
        case _ =>
          ValidationException.fromUndefinedAttribute( b.key )
      }
    }
    if( req - actualReq > 0 ) {
      var allKeys = new mutable.HashSet[String]();
      for( val AttrDecl( key, _, REQUIRED ) <- attrSpec.values )
        allKeys += key;
      for( val a <- attribs )
        allKeys -= a.key;
      ValidationException.fromMissingAttribute( allKeys )
    }
    new AttributeSeq( (attribsTemplate:::attribs):_* )
  }
}
