// test synthesizeSAMFunction: scope hygiene
trait SamFun[T1, R] { self =>
  def apply(v1: T1): R

  // this should type check, as the apply ref is equivalent to self.apply
  // it shouldn't resolve to the sam's apply that's synthesized (that wouldn't type check, hence the pos test)
  def compose[A](g: SamFun[A, T1]): SamFun[A, R] = { x => apply(g(x)) }
}
