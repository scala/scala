 trait Base[@specialized(Double) B] {
  def default: B;
 }

 trait D1 extends Base[Double] {
  override def default = 0.0;
 }

 class D2 extends D1  {
  override def default: Double = 1.0;
 }


object Test extends App {
  val d2 = new D2

  assert(d2.default == 1.0, d2.default)
  assert((d2: Base[_]).default == 1.0, (d2: Base[_]).default)
  assert((d2: D1).default == 1.0, (d2: D1).default)

  println(runtime.BoxesRunTime.integerBoxCount)
}
