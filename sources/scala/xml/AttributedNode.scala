package scala.xml ;

abstract class AttributedNode extends Node {

val attribHashCode:int;
def attributes:Map[String,String];

}
