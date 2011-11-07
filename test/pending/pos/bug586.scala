import scala.collection.immutable.{Map, TreeMap, ListMap, ListSet, Set}
import scala.collection.{immutable=>imm, mutable=>mut}

case class HashTreeSet[A](map: imm.Map[A, Unit])
extends Object 
with imm.Set[A]
{
  def this() = this(null)
  
  def size = map.size
  def +(elem: A) = new HashTreeSet(map + elem -> ())
  def -(elem: A) = new HashTreeSet(map - elem)
  def contains(elem: A) = map.isDefinedAt(elem)
  def iterator = map.iterator.map(._1)
  def empty:imm.Set[A] = new HashTreeSet[A]()
}


abstract class Goal2 {
  type Question
  val question: Question

  type Answer                        
  def initialAnswer: Answer
}



abstract class AbstractRespondersGoal 
extends Goal2 // TYPEFIX -- comment out the extends Goal2
{
}

class RespondersGoal(
    val question: String,
    rcvr: String,
    signature: String,
    codebase: String)
extends AbstractRespondersGoal
{
  type Question = String
  type Answer = imm.Set[String]

  val initialAnswer = new HashTreeSet[String]()// TYPEFIX .asInstanceOf[Answer]
}


class SingleResponderGoal(val question: String, responder: String)
extends AbstractRespondersGoal
{
  type Question = String
  type Answer = Set[String]
  val initialAnswer = (new ListSet[String])//TYPEFIX .asInstanceOf[Answer]
}

class RespondersGoalSet
//extends OneKindGoalSet
{
	type Question = String
  type Answer = imm.Set[String]
  type MyGoal = AbstractRespondersGoal

  var selector: Boolean = _
  def newGoal(question: String)   //TYPEFIX  :MyGoal
  = {

    selector match {
//      case StaticMethodSelector(method: MethodRef) =>
 case true =>
             new SingleResponderGoal(null, null)
        
//      case DynamicMethodSelector(signature: MethodSignature) => {
case false => {
        new RespondersGoal(null, null,null,null)
      }
    }
  }
}
