package scala.tools.dtd2scala ;

import scala.tools.dtd2scala.regexp._ ;

import java.io.PrintWriter ;
import scala.collection.mutable.{Set,Map,HashMap,HashSet} ;

class DeclToSQL(fOut:PrintWriter,mName:String,elemMap:Map[String,ElemDecl ] ) {

  abstract class Vertex {
    var marked = false;
  }
  case class Label( name:String ) extends Vertex {
    def this( n:Node ) = { this( n.name ) };
  };
  case class Attr(name:String) extends Vertex;
  case class Rhs( ename:String, single:Set[Vertex], mult:Set[Vertex] ) {
    var containsText = false;
    def addSingle(n:Node):Unit = addSingle( new Label( n ) );
    def addSingle(v:Vertex):Unit = if( single.contains(v) )
			      { single -= v; mult += v; }
			    else if( !mult.contains(v) )
			      { single += v; };

    def addMult(n:Node):Unit = addMult( new Label( n ));
    def addMult(v:Vertex):Unit = { single -= v; mult += v }
    def this( ename:String ) = {
      this(ename,new HashSet[Vertex],new HashSet[Vertex]);
    }
    override def toString() = { " single: "+single+" mult:"+mult +"\n"}
  };

  // step 1. forget order of children in a node type
  //         retain only cardinality

  val looseMap:Map[String,Rhs] = {
    var rhs:Rhs = null:Rhs;
    def mults( r:RegExp ):Unit = r match {
      case x:Node       => rhs.addMult( x )
      case Star( s )    => mults( s );
      case Seq( rs@_* ) => rs.map( mults ); { }
      case Alt( rs@_* ) => rs.map( mults ); { }
      case _            => ;
    }
    def singles( r:RegExp ):Unit = r match {
      case x:Node       => rhs.addSingle( x )
      case Star( s )    => mults( s );
      case Seq( rs@_* ) => rs.map( singles ); { }
      case Alt( rs@_* ) => rs.map( singles ); { }
      case _            => ;
    }
    val m = new HashMap[String, Rhs];
    for( val decl <- elemMap.values ) do {
      //System.err.print("[processing element "+decl.name
	//		 +" "+decl.contentModel+"]");
      val orig = RegExp.parse( decl.contentModel );
      //System.err.println("[parsing yields "+orig+"]");

      rhs = new Rhs( decl.name );
      singles( orig );
      rhs.containsText = decl.containsText;
      for( val aname <- decl.attribs.keys ) do rhs.addSingle( Attr( aname ));
      m.update( decl.name, rhs );
    }
    m
  }
  def run = {
    fOut.println( "-- this file is generated from a DTD");
    fOut.println( "looseMap "+looseMap);
    fOut.flush();
  }


}

