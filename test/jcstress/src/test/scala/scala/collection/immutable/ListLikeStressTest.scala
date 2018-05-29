package scala.collection.immutable

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.infra.results._

@JCStressTest
@Outcomes(Array(
  new Outcome(id = Array("-1, 0"), expect = Expect.ACCEPTABLE, desc = "Read before write"),
  new Outcome(id = Array("16, 0"), expect = Expect.ACCEPTABLE, desc = "Read after all writes")  
))
@State
class ListLikeStressTest {

  var v: ListLike = _

  @Actor
  def actor1(r: II_Result): Unit = {
    val list = new ListLike("a")
    var l = list
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = new ListLike("a")
    l = l.tail
    l.tail = NilLike
    //java.lang.invoke.VarHandle.releaseFence()
    v = list

  }

  @Actor
  def actor2(r: II_Result): Unit = {
    var l = v
    if (l eq null) {
      r.r1 = -1
      r.r2 = 0
      return
    }
    var len = 0
    var nulls = 0
    while (l ne NilLike) {
      if (l eq null) {r.r1 = len; r.r2 = nulls + 1; return}
      if (l.head eq null) nulls += 1
      assert(l ne l.tail)
      l = l.tail
      len += 1
    }
    r.r1 = len
    r.r2 = nulls

  }

}

class ListLike(var head: AnyRef) {
  var tail: ListLike = _
}

object NilLike extends ListLike(null)
