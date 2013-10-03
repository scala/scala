class S { val q = "" }

class B extends S {
  val x1: B.super.q.type = q
  val x2: B.this.q.type  = q
}
