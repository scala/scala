import Function.unlift

object Test {
  def evens1(x: Int) = if (x % 2 == 0) Some(x) else None
  def evens2: PartialFunction[Int, Int] = {
    case x if x % 2 == 0  => x
  }
  
  def main(args: Array[String]): Unit = {
    val f1 = evens1 _
    val f2 = evens2.lift
    
    assert(1 to 10 forall (x => f1(x) == f2(x)))
    
    val f3 = unlift(f1)
    val f4 = unlift(f2)
    
    assert(1 to 10 forall { x =>
      if (!f3.isDefinedAt(x)) !f4.isDefinedAt(x)
      else f3(x) == f4(x)
    })
    
    assert(f1 eq f3.lift)
    // Hmm, why is this not true:
    // assert(f2 eq f4.lift)
  }
}
