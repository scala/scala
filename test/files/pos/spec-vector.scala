// ticket #3379, abstract overrides
trait Vector extends (Int=>Double) {
  override def apply(i: Int): Double
}
