trait Super[@specialized(Int) A] {
  def superb = 0
}

object Sub extends Super[Int] {
  // it is expected that super[Super].superb crashes, since 
  // specialization does parent class rewiring, and the super
  // of Sub becomes Super$mcII$sp and not Super. But I consider
  // this normal behavior -- if you want, I can modify duplicators
  // to make this work, but I consider it's best to keep this
  // let the user know Super is not the superclass anymore.
  // super[Super].superb - Vlad
  super.superb        // okay    
  override def superb: Int = super.superb // okay
}
