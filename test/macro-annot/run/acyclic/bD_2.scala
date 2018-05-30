package acyclicb

object DD {
  import CC.CC._

  val x = new X

  @pkg.doubler
  object D {
    class X { override def toString = "DX" }
  }
}
