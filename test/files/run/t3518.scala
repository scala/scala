object Test {
  val r1 = 1.0 to 10.0 by 0.5
  val r2 = 1.0 to 1.0 by 1.0
  val r3 = 10.0 to 1.0 by -0.5
  val r4 = 1.0 until 1.0 by 1.0
  val r5 = 1 to 100 by 2
  
  def main(args: Array[String]): Unit = {
    assert(r3 forall (r1 contains _))
    assert(r1 forall (r3 contains _))
    assert(r2.size == 1)
    assert(r4.isEmpty)
    assert(List(1,3,5,97,99) forall (r5 contains _))
    assert(List(2,4,6,98,100) forall (x => !r5.contains(x)))
  }
}
