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
import scala.collection.immutable.TreeSet ;

object AttributeSeq {
  final val Empty = new AttributeSeq { final def sortedSeq = new TreeSet[Attribute] }

  final def fromMap(as:Map[Pair[String,String],String]) = {
    AttributeSeq.fromAttrs( {
      for( val a <- as.keys.toList )
      yield Attribute(a._1,a._2.intern(), as(a))
    }:_* )
  }

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
  final def fromAttrs(as: Attribute*) = {
    var ts = new TreeSet[Attribute];
    for( val a <- as ) {
      if( a.key != "xmlns" ) {
        ts = ts + a ;
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

  override def hashCode():Int = sortedSeq.hashCode();
}

