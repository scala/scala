import scala.collection.mutable

import scala.language.{ reflectiveCalls }

object Test extends App {
  def last0(ml: mutable.MutableList[Int]) =
    ml.asInstanceOf[{def last0: mutable.LinkedList[Int]}].last0

  def first0(ml: mutable.MutableList[Int]) =
    ml.asInstanceOf[{def first0: mutable.LinkedList[Int]}].first0

  val f = mutable.Queue[Int]()
  def check(desc: String) {
    assert(f.length == 0, s"$desc: non empty: $f")
    assert(last0(f).isEmpty, s"$desc: last0 leak: ${last0(f)}")
    assert(first0(f).isEmpty, s"$desc: first0 leak: ${last0(f)}")
  }

  f.enqueue(1)
  f.dequeue()
  check("dequeue 1")

  f.enqueue(1)
  f.enqueue(2)
  f.dequeue()
  assert(last0(f).toList == List(2), last0(f))
  f.dequeue()
  check("dequeue 2")

  f.enqueue(1)
  f.dequeueAll(_ => false)
  f.dequeueAll(_ => true)
  check("dequeueAll")

  f.enqueue(1)
  f.dequeueFirst(_ => true)
  check("dequeueFirst")

  {
    f.enqueue(1)
    val tail = f.tail
    assert(last0(tail).isEmpty, last0(tail))
    assert(first0(tail).isEmpty, first0(tail))
  }

  {
    val ml = mutable.MutableList[Int]()
    1 +=: ml
    val tail = ml.tail
    assert(last0(tail).isEmpty, last0(tail))
    assert(first0(tail).isEmpty, first0(tail))
  }

  {
    val ml = mutable.MutableList[Int]()
    1 +=: ml
    ml += 2
    val tail = ml.tail
    assert(last0(tail).toList == List(2), last0(tail))
    assert(first0(tail) == last0(tail).toList, first0(tail))
    assert(last0(tail.tail).toList == Nil, last0(tail.tail).toList)
    assert(first0(tail.tail) == Nil, first0(tail.tail))
  }
}
