/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

import java.lang.StringBuffer ; /* Java dependency! */
import scala.collection.Map ;

/** Class for pretty printing. After instantiating, you can use the
 *  toPrettyXML methods to convert XML to a formatted string. The class
 *  can be reused to pretty print any number of XML nodes.
 *
 * @param width the width to fit the output into
 * @step  indentation
**/

class PrettyPrinter( width:Int, step:Int ) {

  class BrokenException() extends java.lang.Exception();

  class Item ;
  case object Break extends Item {
    override def toString() = "\\";
  };
  case class Box( col:Int, s:String ) extends Item;
  case class Para( s:String ) extends Item;

  var items:List[Item] = Nil;

  var cur = 0;

  def reset() = {
    cur = 0;
    items = Nil;
  }

  /* try to cut at whitespace */
  def cut( s:String, ind:Int ):List[Item] = {
    val tmp = width - cur;
    if( s.length() < tmp )
      return List(Box(ind,s));
    val sb = new StringBuffer();
    var i = s.indexOf(' ');
    if( i > tmp ) throw new BrokenException(); // cannot break

    var last = i::Nil;
    while( i < tmp ) {
      last = i::last;
      i = s.indexOf(' ', i );
    }
    var res:List[Item] = Nil;
    while( Nil != last ) try {
      val b = Box( ind, s.substring( 0, last.head ));
      cur = ind;
      res = b :: Break :: cut( s.substring( last.head, s.length()), ind );
       // backtrac
    } catch {
      case _:BrokenException => last = last.tail;
    }
    throw new BrokenException()
  }

  /** try to make indented box, if possible, else para */
  def makeBox( ind:Int, s:String )  = {
    if( cur < ind )
      cur == ind;
    if( cur + s.length() > width ) {            // fits in this line
      items = Box( ind, s ) :: items;
      cur = cur + s.length()
    } else try {
      for( val b <- cut( s, ind ).elements )  // break it up
        items = b :: items
    } catch {
      case _:BrokenException => makePara( ind, s ); // give up, para
    }
  }

  // dont respect indent in para, but afterwards
  def makePara( ind:Int, s:String ) = {
    items = Break::Para( s )::Break::items;
    cur = ind;
  }

  // respect indent
  def makeBreak() = { // using wrapping here...
    items = Break::items;
    cur = 0;
  }

  def leafTag( n:Node ) = {
    val sb = new StringBuffer("<");
    sb.append( n.label );
    Utility.attr2xml(  n.attribute.elements, sb );
    sb.append("/>");
    sb.toString();
  }

  def startTag( n:Node ) = {
    val sb = new StringBuffer("<");
    sb.append( n.label );
    Utility.attr2xml(  n.attribute.elements, sb );
    sb.append('>');
    sb.toString();
  }

  /* returns a formatted string containing well-formed XML
  **/
  def format( n:Node ):String = {
    reset();
    traverse( n, 0 );
    val sb = new StringBuffer();
    var cur = 0;
    //Console.println( items.reverse );
    for( val b <- items.reverse ) b match {
      case Break =>
        sb.append('\n');  // on windows: \r\n
        cur = 0;
      case Box(i, s) =>
        while( cur < i ) {
          sb.append(' ');
          cur = cur + 1;
        }
        sb.append( s );
      case Para( s ) =>
        sb.append( s );
    }
    sb.toString();
  }

  /* returns a formatted string containing well-formed XML nodes.
  **/
  def format( ns:Seq[Node] ):String = {
    var sb2 = new StringBuffer();
    for( val n <- ns.elements ) {
      sb2.append( format( n ))
    }
    sb2.toString();
  }

  def breakable( n:Node ):boolean = {
    val it = n.child.elements;
    while( it.hasNext )
      it.next match {
        case _:Text | _:CharData | _:Comment | _:EntityRef | _:ProcInstr =>
        case _:Node => return true;
      }
    return false
  }
    /** @param tail: what we'd like to sqeeze in */
    def traverse( node:Node, ind:int ):Unit = {
      node match {

        case _:Text | _:CharData | _:Comment | _:EntityRef | _:ProcInstr =>
          makeBox( ind, node.toString() );

        case _:Node =>
          val test = node.toString();

        if( ( test.length() < width - cur ) // all ?
          &&( !breakable( node ))) {
            makeBox( ind, test );
          } else {  // start tag + content + end tag
            //Console.println(node.label+" ind="+ind);
            val stg    = startTag( node );
            val endTag = "</"+node.label+">";
            val len2   = node.label.length() + 1;

            if( stg.length() < width - cur ) { // start tag fits

              makeBox( ind, stg );
              makeBreak();
              traverse( node.child.elements, ind + step );
              makeBox( ind, endTag );

            } else if( len2 < width - cur ) {
              // <start label + attrs + tag + content + end tag
              makeBox( ind, stg.substring( 0,    len2 ));
              makeBreak();
              makeBox( ind, stg.substring( len2, stg.length() ));
              makeBreak();
              traverse( node.child.elements, ind + step );
              makeBox( cur, endTag );
            }
          }
        }
    }

  def traverse( it:Iterator[Node], ind:int ):unit = {
    for( val c <- it ) {
      traverse( c, ind );
      makeBreak();
    }
  }

}
