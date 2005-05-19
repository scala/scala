package scala.xml.dtd ;


object ContentModel extends scala.util.regexp.WordExp {
  type _labelT = ElemName;
  type _regexpT = RegExp;

  case class ElemName(name: String) extends Label {
    override def toString() = "ElemName(\""+name+"\")";
  }

  def parse(s: String): ContentModel = Parser.parse( s );

  /*
  def isMixed(alt: Alt): Boolean = {
    val it = alt.rs.elements;
    it.next == PCDATA_ && {
      while( it.hasNext && it.next.isInstanceOf[Letter] ) {} ;
      !it.hasNext
    }
  }
  */

  def getLabels(r: RegExp): scala.collection.Set[String] = {
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

  /* precond: rs.length >= 1 */
  private def toString(rs: Seq[RegExp], sb: StringBuffer, sep: Char): Unit = {
    val it = rs.elements;
    toString(it.next, sb);
    for(val z <- it) {
      sb.append( sep );
      toString( z, sb );
    }
  }

  def toString(c: ContentModel, sb: StringBuffer): StringBuffer = c match {

      case ANY    =>
        sb.append("ANY");

      case EMPTY    =>
        sb.append("EMPTY");

      case PCDATA =>
        sb.append("(#PCDATA)");

      case ELEMENTS( r ) =>
        toString(r, sb)

      case MIXED( r ) =>
        sb.append("(#PCDATA"); toString(r, sb); sb.append( ')' )

  }

  def toString(r: RegExp, sb:StringBuffer): StringBuffer = {
    r match {
      case Eps     =>
        sb

      case Sequ(rs @ _*) =>
        sb.append( '(' ); toString(rs, sb, ','); sb.append( ')' );

      case Alt(rs @ _*) =>
        sb.append( '(' ); toString(rs, sb, '|'); sb.append( ')' );

      case Star(r: RegExp) =>
        sb.append( '(' ); toString(r, sb); sb.append( ")*" );

      case Letter(ElemName(name)) =>
        sb.append(name);

    }
  }
}

sealed abstract class ContentModel {
  override def toString(): String = {
    val sb = new StringBuffer();
    toString(sb);
    sb.toString();
  }

  def toString(sb:StringBuffer): StringBuffer;
}

case object PCDATA extends ContentModel {
  def toString(sb:StringBuffer): StringBuffer = sb.append("(#PCDATA)");
}
case object EMPTY extends ContentModel {
  def toString(sb:StringBuffer): StringBuffer = sb.append("EMPTY");
}
case object ANY extends ContentModel {
  def toString(sb:StringBuffer): StringBuffer = sb.append("ANY");
}

case class  MIXED(r:ContentModel.RegExp) extends ContentModel {
  def toString(sb:StringBuffer): StringBuffer =  {
    sb.append("(#PCDATA|");
    ContentModel.toString(r, sb);
    sb.append(")*");
  }
}

case class  ELEMENTS(r:ContentModel.RegExp) extends ContentModel {
  def toString(sb:StringBuffer): StringBuffer =
    ContentModel.toString(r, sb);

}
