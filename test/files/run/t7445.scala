/*
 * Since ListMap is an ordered collection, head and tail should also respect the order, see https://issues.scala-lang.org/browse/SI-7445
 */
import scala.collection.immutable.ListMap
import scala.collection.mutable.{ListMap => MListMap}
object Test extends App{
  val subject = ListMap(1->1,2->2,3->3,4->4,5->5)
  assert(subject.tail+subject.head == subject)
  val msubject = MListMap(1->1,2->2,3->3,4->4,5->5)
  assert(subject.head == msubject.head)
}
