package scala.xml.dtd ;

/** contains parse method to parse regexp from content model */
object RegExp {
  /** parse a regular expression from a DTD content model */
  def parse(s:String):RegExp = Parser.parse( s );
}

/** abstract super class of regular expressions for DTD content models */
abstract class RegExp {
  def toRegExp(): String;
  def getLabels: scala.collection.Set[String] = {
    val s = new scala.collection.mutable.HashSet[String]();
    def traverse1(xs: Seq[RegExp]): Unit = {
      val it = xs.elements;
      while( it.hasNext )
        traverse( it.next );
    }
    def traverse(r: RegExp): Unit = {
      r match {
        case RNode( name ) => s += name;
        case Star( x @ _ ) => traverse( x ); // bug if x@_*
        case Sequ( xs @ _* ) => traverse1(xs);
        case Alt( xs @ _* )  => traverse1(xs);
      }
    }
    traverse( this );
    return s
  }
}


case class RNode( name:String ) extends RegExp {
  final def toRegExp() = name;
  final override def toString() = {
    val sb = new StringBuffer("RNode(\"");
    sb.append(name);
    sb.append('"');
    sb.append(')');
    sb.toString()
  }
};
case object PCDATA_ extends RegExp {
  final def toRegExp() = "#PCDATA";
  override def toString() = "PCDATA_";
}
case object ANY_    extends RegExp {
  final def toRegExp() = "ANY";
  override def toString() = "ANY_";
}
case object Eps extends RegExp {
  final def toRegExp() = "()";
  override def toString() = "Eps";
}
case class Star(r:RegExp) extends RegExp {
  final def toRegExp() = r.toRegExp()+"*";
}
/** rs should be not empty */
case class Sequ(rs:RegExp*) extends RegExp {
  final def toRegExp() = {
    val it = rs.elements;
    val sb = new StringBuffer();
    sb.append('(');
    sb.append( it.next.toRegExp() );
    for( val z <- it ) {
      sb.append( ',' );
      sb.append( z.toRegExp() );
    }
    sb.append( ')' );
    sb.toString();
  }
  final override def toString() = {
    val it = rs.elements;
    val sb = new StringBuffer("Alt(");
    sb.append( it.next.toString() );
    for( val z <- it ) {
      sb.append( ',' );
      sb.append( z.toString() );
    }
    sb.append( ')' );
    sb.toString();
  }}
/** rs should be not empty */
case class Alt(rs:RegExp*) extends RegExp {
  final def mixed:boolean = {
    val it = rs.elements;
    ( it.next == PCDATA_ ) && it.forall { x:RegExp => x.isInstanceOf[RNode] }
  }
  final def toRegExp() = {
    val it = rs.elements;
    val sb = new StringBuffer();
    sb.append('(');
    sb.append( it.next.toRegExp() );
    for( val z <- it ) {
      sb.append( '|' );
      sb.append( z.toRegExp() );
    }
    sb.append(')');
    sb.toString();
  }
  final override def toString() = {
    val it = rs.elements;
    val sb = new StringBuffer("Alt(");
    sb.append( it.next.toString() );
    for( val z <- it ) {
      sb.append( ',' );
      sb.append( z.toString() );
    }
    sb.append( ')' );
    sb.toString();
  }
}
