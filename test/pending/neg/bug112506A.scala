package test;
import scala.collection.immutable.ListSet
import scala.collection.mutable._;
trait TypeManagerXXX {
  trait TypedNode;
  type Node;
}
trait ScalaTyperXXX extends TypeManagerXXX {
  private var typed : Node = null;
  private val dependMap = new HashMap[String,ListSet[TypedNode]]; 
  override def lookupEntry(name: String): String = {
    val set = dependMap.get(name) match {
    case Some(set) => set;
    case None => new ListSet[Node]
    }
    dependMap.update(name, set + typed);
    throw new Error;
  }
}
