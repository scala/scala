class M1
class M2 extends M1
class M3 extends M2

trait S1 { val q = new M1 ; val q1: q.type = q }
trait S2 { val q = new M2 ; val q2: q.type = q }

class B extends S1 with S2 {
  override val q = new M3
  val q3: q.type = q

  var x1: B.super[S1].q1.type = null
  var x2: B.super[S2].q2.type = null
  var x3: B.this.q3.type      = null

  x1 = x1
  x1 = x2
  x1 = x3
  x2 = x1
  x2 = x2
  x2 = x3
  x3 = x1
  x3 = x2
  x3 = x3

  x1 = q1
  x1 = q2
  x1 = q3
  x2 = q1
  x2 = q2
  x2 = q3
  x3 = q1
  x3 = q2
  x3 = x3
}

class C extends S1 with S2 {
  override val q = new M3
  val q3: q.type = q

  // x1's type and x2's type are incompatible
  // x3's is assignable to x1 or x2, but not vice versa
  var x1: C.super[S1].q.type = null
  var x2: C.super[S2].q.type = null
  var x3: C.this.q.type      = null

  x1 = x1
  x1 = x2  // fail
  x1 = x3
  x2 = x1  // fail
  x2 = x2
  x2 = x3
  x3 = x1  // fail
  x3 = x2  // fail
  x3 = x3

  x1 = q1
  x1 = q2
  x1 = q3
  x2 = q1
  x2 = q2
  x2 = q3
  x3 = q1
  x3 = q2
  x3 = x3

  x1 = q
  x1 = super[S1].q
  x1 = super[S2].q  // fail
  x2 = q
  x2 = super[S1].q  // fail
  x2 = super[S2].q
  x3 = q
  x3 = super[S1].q  // fail
  x3 = super[S2].q  // fail
}
