package scala.xml.nobinding;

import scala.collection.Map ;
import scala.collection.immutable.ListMap ;

/** an XML node. use this when data binding is not desired.
**/
class Element( name:String, ch: Seq[Node] ) extends AttributedNode with Symbol( name, ch )  {

  def label = name;
  def children = ch;

  override def attributes : Map[String,String] = ListMap.Empty[String,String] ;
  override val attribHashCode:int = 0;
}

