package scala.xml.dtd ;

import scala.collection.mutable;
import scala.collection.Set;
import scala.collection.Map;
import ContentModel._ ;

class ElementValidator(namespace$$: String, elemSpec: RegExp) {
  final val namespace = namespace$$.intern();

  var autom:Autom = _;

  trait Autom {
    def run(s:Seq[Node]):Unit;
  }

  object TrivialAutom extends Autom {
    def run(s:Seq[Node]):Unit = {};
  }

  object EmptyAutom extends Autom {
    /** "not even entity references, PIs, etc" */
    def run(s:Seq[Node]):Unit =
      if( s.length != 0 )
        throw MakeValidationException.fromNonEmptyElement();

  }

  object TextAutom extends Autom {
    /** only text nodes, entity references, PIs, comments, etc" */
    def run(s:Seq[Node]):Unit = {
      val it = s.elements;
      while( it.hasNext ) {
        val n = it.next;
        if( n.typeTag$ >= 0 )
           throw MakeValidationException.fromUndefinedElement( n.label );
      }
    }
  }

  class MixedModeAutom(set:scala.collection.Set[String]) extends Autom {
    def run(s:Seq[Node]):Unit = {
      val it = s.elements;
      while( it.hasNext ) {
        val n = it.next;
        if(( n.namespace == namespace )&& !set.contains( n.label ))
          throw MakeValidationException.fromUndefinedElement( n.label );
      }
    }
  }

  autom = elemSpec match {
    case ANY_ => TrivialAutom;
    case Eps  => EmptyAutom;
    case PCDATA_ | Sequ( PCDATA_ ) => TextAutom
    case Star(z:Alt) if(( z.rs.length == 1 )&& (z.rs(0) match {
      case PCDATA_   => true
      case _         => false
    })) => new MixedModeAutom(getLabels( elemSpec ));
    case _    => TrivialAutom;
  }
  def validate( nodes:Seq[Node] ):Unit = autom.run( nodes );
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
              MakeValidationException.fromFixedAttribute( key, attValue, b.value );
            else
              add( attribs, b )
          case REQUIRED =>
            actualReq = actualReq + 1;
          attribs = add( attribs, b )
          case _ =>
            attribs = add( attribs, b )
        }
        case _ =>
          MakeValidationException.fromUndefinedAttribute( b.key )
      }
    }
    if( req - actualReq > 0 ) {
      var allKeys = new mutable.HashSet[String]();
      for( val AttrDecl( key, _, REQUIRED ) <- attrSpec.values )
        allKeys += key;
      for( val a <- attribs )
        allKeys -= a.key;
      MakeValidationException.fromMissingAttribute( allKeys )
    }
    new AttributeSeq( (attribsTemplate:::attribs):_* )
  }
}
