package scala.xml.dtd ;

/** contains parse method to parse regexp from content model */
object RegExp {
  /** parse a regular expression from a DTD content model */
  def parse(s:String):RegExp = Parser.parse( s );
}

/** abstract super class of regular expressions for DTD content models */
abstract class RegExp ;


case class RNode( name:String ) extends RegExp {
  override def toString() = name;
};
case object PCDATA_ extends RegExp {
  override def toString() = "#PCDATA";
}
case object ANY_    extends RegExp {
  override def toString() = "ANY";
}
case object Eps extends RegExp {
  override def toString() = "()";
}
case class Star(r:RegExp) extends RegExp {
  override def toString() = r.toString()+"*";
}
/** rs should be not empty */
case class Sequ(rs:RegExp*) extends RegExp {
  override def toString() = {
    val it = rs.elements;
    val sb = new StringBuffer();
    sb.append('(');
    sb.append( it.next.toString() );
    for( val z <- it ) {
      sb.append( ',' );
      sb.append( z.toString() );
    }
    sb.append( ')' );
    sb.toString();
  }
}
/** rs should be not empty */
case class Alt(rs:RegExp*) extends RegExp {
  final def mixed:boolean = {
    val it = rs.elements;
    ( it.next == PCDATA_ ) && it.forall { x:RegExp => x.isInstanceOf[RNode] }
  }
  override def toString() = {
    val it = rs.elements;
    val sb = new StringBuffer();
    sb.append('(');
    sb.append( it.next.toString() );
    for( val z <- it ) {
      sb.append( '|' );
      sb.append( z.toString() );
    }
    sb.append(')');
    sb.toString();
  }
}
