/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

import scala.collection.Map ;
import scala.collection.mutable ;
import scala.collection.immutable.TreeSet ;

object AttributeSeq {
  final val Empty = new AttributeSeq { final def sortedSeq = new TreeSet[Attribute] }

  /** construct an attribute sequence from a map
   *  @param as a map from Pair(uri,key) to value
   */
  final def fromMap(as:Map[Pair[String,String],String]) = {
    AttributeSeq.fromAttrs( {
      for( val a <- as.keys.toList )
      yield Attribute(a._1,a._2.intern(), as(a))
    }:_* )
  }

  /** construct an attribute sequence from a map
   *  @param as a map from Pair(uri,key) to value
   */
  final def fromUMap(as:Map[UName,String]) = {
    AttributeSeq.fromAttrs( {
      for( val a <- as.keys.toList )
      yield Attribute(a.uri,a.label, as(a))
    }:_* )
  }

  /** construct from a map, fixing namespacePs to ns
   *  each Attribute with an empty namespace will get the namespace ns.
   *  @param ns the namespace to use instead of the empty one
   *  @param as a map from Pair(uri,key) to value
   */
  final def fromMap(ns:String, as:Map[Pair[String,String],String]) = {
    AttributeSeq.fromAttrs( {
      for( val a <- as.keys.toList )
      yield {
        val res =
          if( a._1.length() == 0 )
            Attribute(ns, a._2.intern(), as( a ))
          else
            Attribute(a._1.intern(), a._2.intern(), as(a));
        res
      }
    }:_*)
  }
  /** construct from sequence of Attribute.
   * @param as any sequence of attributes a1,a2,...
   */
  final def fromAttrs(as: Attribute*) = {
    var ts = new TreeSet[Attribute];
    for( val a <- as ) {
      if( a.key != "xmlns" ) {
        ts = ts + a ;
      }
    }
    new AttributeSeq { final def sortedSeq = ts };
  }

  /** construct from sequence of Attribute, fixing namespaces to ns
   *  Each Attribute with an empty namespace will get the namespace ns.
   * @param ns the namespace to use instead of the empty one
   * @param as any sequence of attributes a1,a2,...
   */
  final def fromAttrs(ns:String, as: Attribute*) = {
    var ts = new TreeSet[Attribute];
    for( val a <- as ) {
      if( a.key != "xmlns" ) {
        var url: String = _;
        if ( a.namespace.length() == 0) url = ns else url = a.namespace;
        ts = ts + Attribute(url,a.key,a.value) ;
      }
    }
    new AttributeSeq { final def sortedSeq = ts };
  }
}

/** Sorted linear list of XML attributes.
 *  An attribute seq does *not* contain namespace defining attributes
 *  like xmlns or xmlns:pref
 *  @author  Burak Emir
 */
abstract class AttributeSeq with Seq[Attribute] {

  def sortedSeq:TreeSet[Attribute];

  override def equals(x: Any) = x match {
    case that:AttributeSeq => (this.sortedSeq == that.sortedSeq)
    case _                 => false;
  }


  final def length = sortedSeq.size;
  final def elements = sortedSeq.elements;
  final def apply(i:Int) = sortedSeq.elements.drop(i).next;

  def lookup(ns:String, key:String):Option[Attribute] = {
    val it = sortedSeq.elements;
    while( it.hasNext ) {
      val a = it.next;
      if( a.key > key ) return None
      else if( a.key == key ) {
        if( a.namespace > ns ) return None
        else if( a.namespace == ns ) return Some(a)
      }
    }
    return None;
  }

  /** Return a new AttributeSeq with removed attributes
   *  Only namespace and label are regarded
   *
   *  @param  attrs the attributes to be removed
   *  @return a new AttributeSeq without attributes from attrs
   */
  final def -(attrs: Attribute*) = {
    val diff = new mutable.HashSet[Pair[String,String]];
    for(val a <- attrs) {
      diff += Pair(a.namespace, a.key);
    }
    var newset = new TreeSet[Attribute];
    for(val a <- elements; !(diff contains Pair(a.namespace, a.key))) {
      newset = newset + a;
    }
    new AttributeSeq { final def sortedSeq = newset };
  }

  /** Return a new AttributeSeq with updated or added attributes
   *
   *  @param attrs
   *  @return a new symbol with updated attributes
   */
  final def %(attrs: Attribute*) =
    AttributeSeq.fromAttrs((elements.toList ::: attrs.elements.toList):_*);

  final def map(f: Attribute => Attribute): AttributeSeq = {
    AttributeSeq.fromAttrs( elements.map( f ).toList:_* )
  }

  override def hashCode():Int =
    sortedSeq.hashCode();

  def toMap:Map[Pair[String,String],Attribute] =
    new Map[Pair[String,String],Attribute] {
      def elements = new Iterator[Pair[Pair[String,String],Attribute]] {
        val it = AttributeSeq.this.sortedSeq.elements;
        def hasNext = it.hasNext;
        def next = { val a = it.next; Pair(Pair(a.namespace,a.key),a) }
      }
      def size = AttributeSeq.this.length;
      def get(p:Pair[String,String]) = AttributeSeq.this.lookup(p._1, p._2);
    }
}

