/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

import scala.collection.mutable.HashMap ;
import scala.collection.immutable.TreeSet ;

object AttributeSeq {
  final val Empty = new AttributeSeq();

  final def fromHashMap(as:HashMap[Pair[String,String],String]) = {
    new AttributeSeq( {
      for( val a <- as.keys.toList )
      yield Attribute(a._1,a._2, as(a))
    }:_* )
  }
}

/** Sorted linear list of XML attributes.
 *  An attribute seq does *not* contain namespace defining attributes
 *  like xmlns or xmlns:pref
 *  @author  Burak Emir
 */
class AttributeSeq( as:Attribute* ) with Seq[Attribute] {

  private var treeSet:TreeSet[Attribute] = new TreeSet[Attribute];

  for( val a <- as ) {
    if( a.key != "xmlns" ) {
      treeSet = treeSet + a ;
    }
  }
  final def length = treeSet.size;
  final def elements = treeSet.elements;
  final def apply(i:Int) = treeSet.elements.drop(i).next;

  def lookup(ns:String, key:String):Option[Attribute] = {
    val it = treeSet.elements;
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

  /** Return a new AttributeSeq with updated or added attributes
   *
   *  @param attrs
   *  @return a new symbol with updated attributes
   */
  final def %(attrs: Attribute*) =
    new AttributeSeq((elements.toList ::: attrs.elements.toList):_*);

  final def map(f: Attribute => Attribute): AttributeSeq = {
    new AttributeSeq( elements.map( f ).toList:_* )
  }
}

