package scala.xml.dtd ;

abstract class ElemNames          extends scala.util.regexp.Alphabet;
case class ElemName(name: String) extends ElemNames {
  override def toString() = "ElemName(\""+name+"\")";
}

object ContentModel extends scala.util.regexp.WordExp[ElemNames] {
  type regexp = RegExp;

  case object PCDATA_ extends RegExp {
    override def toString() = "PCDATA_";
  }

  case object ANY_    extends RegExp {
    override def toString() = "ANY_";
  }

  def parse(s: String): RegExp = Parser.parse( s );

  def isMixed(alt:Alt):boolean = {
    val it = alt.rs.elements;
    it.next == PCDATA_ && {
      while( it.hasNext && it.next.isInstanceOf[Letter] ) {} ;
      !it.hasNext
    }
  }

  def getLabels(r:RegExp): scala.collection.Set[String] = {
    val s = new scala.collection.mutable.HashSet[String]();
    def traverse1(xs: Seq[RegExp]): Unit = {
      val it = xs.elements;
      while( it.hasNext )
        traverse( it.next );
      }
    def traverse(r: RegExp): Unit = {
      r match {
        case Letter(ElemName( name )) => s += name;
        case Star(  x @ _  ) => traverse( x ); // bug if x@_*
        case Sequ( xs @ _* ) => traverse1(xs);
        case Alt(  xs @ _* ) => traverse1(xs);
      }
    }
    traverse( r );
    return s
  }

  def toString(r: RegExp):String = {
    val sb = new StringBuffer();
    toString(r, sb);
    sb.toString();
  }

  /* precond: rs.length > 1 */
  private def toString(rs: Seq[RegExp], sb: StringBuffer):Unit = {
    val it = rs.elements;
    sb.append('(');
    toString(it.next, sb);
    for(val z <- it) {
      sb.append( ',' );
      toString( z, sb );
    }
    sb.append( ')' );
  }

  def toString(r: RegExp, sb:StringBuffer):Unit = {
    r match {
      case PCDATA_ => sb.append("PCDATA_");
      case ANY_    => sb.append("ANY_");
      case Eps     => sb.append("Eps");
      case Sequ(rs @ _*) =>
        sb.append("Sequ");
        toString(rs, sb);
      case Alt(rs @ _*) =>
        sb.append("Alt");
        toString(rs, sb);
      case Star(r:RegExp) =>
        sb.append("Star(");
        toString(r, sb);
        sb.append(')');
      case Letter(ElemName(name)) =>
        sb.append("Letter(ElemName(\"");
        sb.append(name);
        sb.append("\"))");
    }
  }
}
