trait T[T] { def apply(x: Int): T }
class C(val x: Int) extends AnyVal { override def toString = s"$x" }

object Test {
  def main(args: Array[String]): Unit = {
    {
      val t: A[String] = s => s
      assert((t: A[_]).apply("there") == "there")
    }
    {
      var u = 0
      val t: T[Unit] = x => u = x
      t.apply(1)
      assert(u == 1)
    }
    {
      val t: T[C] = x => new C(x)
      assert(t.apply(1) == new C(1))
    }
  }
}
