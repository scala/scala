package scala.xml ;

abstract class AttributedNode extends Node {

val attribHashCode:int;
def attributes:scala.collection.Map[String,String];
def toXML = Utility.toXML(this);

}
