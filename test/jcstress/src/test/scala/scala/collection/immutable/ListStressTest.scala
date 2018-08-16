package scala.collection.immutable

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.infra.results.I_Result

@JCStressTest
@Outcomes(Array(
  new Outcome(id = Array("0"), expect = Expect.ACCEPTABLE, desc = "Read before write"),
  new Outcome(id = Array("1"), expect = Expect.ACCEPTABLE, desc = "Read after all writes"),
  new Outcome(id = Array("2"), expect = Expect.FORBIDDEN, desc = "Interleaved read / write")  
))
@State
class ListStressTest {

  var v: List[AnyRef] = _

  var elem1: AnyRef = _
  var elem2: AnyRef = _
  var nil: AnyRef = _

  @Actor
  def actor1(): Unit = {
    val builder = List.newBuilder[String]
    builder += "a"
    builder += "b"
    v = builder.result
  }

  @Actor
  def actor2(): Unit = {
    val v1 = v
    if (v1 ne null) {
      elem1 = v1.head
      val tail = v1.tail
      elem2 = tail.head
      nil = tail.tail
    }
  }

  @Arbiter
  def arbiter(r: I_Result): Unit = {
    r.r1 = 
      if (elem2 == null && elem2 == null && nil == null) 0
      else if (elem1 == "a" && elem2 == "b" && nil == Nil) 1
      else 2
  }
}
