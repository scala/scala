trait T[A] { trait U { type W = A; val x = 3 } }

object Test {
  val x : ({ type V = T[this.type] })#V = null
  val y = new x.U { }
}
