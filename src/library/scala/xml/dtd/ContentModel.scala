/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.dtd

import scala.util.regexp.WordExp
import scala.util.automata.{DetWordAutom, SubsetConstruction, WordBerrySethi}

object ContentModel extends WordExp {
  type _labelT = ElemName
  type _regexpT = RegExp

  object Translator extends WordBerrySethi {
    override val lang: ContentModel.this.type = ContentModel.this
  }

  case class ElemName(name: String) extends Label {
    override def toString() = "ElemName(\""+name+"\")"
  }

  def isMixed(cm: ContentModel) = cm.isInstanceOf[MIXED]
  def containsText(cm: ContentModel) = (cm == PCDATA) || isMixed(cm)

  def parse(s: String): ContentModel = ContentModelParser.parse(s)

  def getLabels(r: RegExp): scala.collection.Set[String] = {
    val s = new scala.collection.mutable.HashSet[String]()
    def traverse1(xs: Seq[RegExp]) {
      val it = xs.elements
      while (it.hasNext) traverse(it.next)
    }
    def traverse(r: RegExp) {
      r match { // !!! check for match translation problem
        case Letter(ElemName(name)) => s += name
        case Star(  x @ _  ) => traverse( x ) // bug if x@_*
        case Sequ( xs @ _* ) => traverse1(xs)
        case Alt(  xs @ _* ) => traverse1(xs)
      }
    }
    traverse(r)
    return s
  }

  def buildString(r: RegExp): String = {
    val sb = new StringBuilder()
    buildString(r, sb)
    sb.toString()
  }

  /* precond: rs.length >= 1 */
  private def buildString(rs: Seq[RegExp], sb: StringBuilder, sep: Char) {
    val it = rs.elements
    val fst = it.next
    buildString(fst, sb)
    for (z <- it) {
      sb.append(sep)
      buildString(z, sb)
    }
    sb
  }

  def buildString(c: ContentModel, sb: StringBuilder): StringBuilder = c match {
    case ANY =>
      sb.append("ANY")
    case EMPTY =>
      sb.append("EMPTY")
    case PCDATA =>
      sb.append("(#PCDATA)")
    case ELEMENTS( _ ) | MIXED( _ ) =>
      c.buildString(sb)
  }

  def buildString(r: RegExp, sb:StringBuilder): StringBuilder = r match {  // !!! check for match translation problem
    case Eps =>
      sb
    case Sequ(rs @ _*) =>
      sb.append( '(' ); buildString(rs, sb, ','); sb.append( ')' )
    case Alt(rs @ _*) =>
      sb.append( '(' ); buildString(rs, sb, '|');  sb.append( ')' )
    case Star(r: RegExp) =>
      sb.append( '(' ); buildString(r, sb); sb.append( ")*" )
    case Letter(ElemName(name)) =>
      sb.append(name)
  }

}

sealed abstract class ContentModel {

  override def toString(): String = {
    val sb = new StringBuilder()
    buildString(sb)
    sb.toString()
  }

  def buildString(sb:StringBuilder): StringBuilder;
  /*
  def validate(cs: NodeSeq): Boolean = this.match {
    case ANY         => true
    case EMPTY       => cs.length == 0
    case PCDATA      => cs.length == 0
                     || (cs.length == 1 && cs(0).isInstanceOf[Text])
    case m@MIXED(r)    => m.runDFA(cs)
    case e@ELEMENTS(r) => e.runDFA(cs)
  }
  */
}

case object PCDATA extends ContentModel {
  override def buildString(sb: StringBuilder): StringBuilder = sb.append("(#PCDATA)")
}
case object EMPTY extends ContentModel {
  override def buildString(sb: StringBuilder): StringBuilder = sb.append("EMPTY")
}
case object ANY extends ContentModel {
  override def buildString(sb: StringBuilder): StringBuilder = sb.append("ANY")
}
sealed abstract class DFAContentModel extends ContentModel {
  import ContentModel.ElemName
  def r: ContentModel.RegExp
  private var _dfa: DetWordAutom[ContentModel.ElemName] = null

  def dfa = {
    if (null == _dfa) {
      val nfa = ContentModel.Translator.automatonFrom(r, 1);
      _dfa = new SubsetConstruction(nfa).determinize;
    }
    _dfa
  }
}
case class MIXED(r: ContentModel.RegExp) extends DFAContentModel {
  import ContentModel.{Alt, Eps, RegExp}
  /*
  def getIterator(ns:NodeSeq) = new Iterator[String] {
    def cond(n:Node) =
      !n.isInstanceOf[Text] && !n.isInstanceOf[SpecialNode];
Console.println("ns = "+ns);
    val jt = ns.elements;
    def hasNext = jt.hasNext;
    def next = {
      var r: Node = jt.next;
      while(!cond(r) && jt.hasNext)  {
        Console.println("skipping "+r);
        r = jt.next;
      }
      Console.println("MIXED, iterator.next, r = "+r);
      if(Text("") == r)
        null
      else
        r.label
    }
  }
  */
  override def buildString(sb: StringBuilder): StringBuilder =  {
    sb.append("(#PCDATA|")
    ContentModel.buildString(Alt(r.asInstanceOf[Alt].rs.toList.drop(1):_*):RegExp, sb);
    sb.append(")*");
  }
}

case class  ELEMENTS(r:ContentModel.RegExp) extends DFAContentModel {
  /*
  def getIterator(ns:NodeSeq) = new Iterator[String] {
    val jt = ns.elements.buffered;
    def hasNext = jt.hasNext;
    def next = {
      var r: Node = jt.next;
      while(r.isInstanceOf[SpecialNode] && jt.hasNext) {
        r = jt.head;
        jt.next;
      }
      Console.println("MIXED, iterator.next, r = "+r);
      if(r.isInstanceOf[Text])
        throw ValidationException("Text not allowed here!")
      else
        r.label
    }
  }
  */
  override def buildString(sb: StringBuilder): StringBuilder =
    ContentModel.buildString(r, sb)
}
