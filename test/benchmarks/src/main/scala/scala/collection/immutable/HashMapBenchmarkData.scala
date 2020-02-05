package scala.collection.immutable


object HashMapBenchmarkData {
  def apply(hashCode: Int, data: String) = new HashMapBenchmarkData(hashCode, data.intern())
}
class HashMapBenchmarkData private (override val hashCode: Int, val data: String) {
  override def equals(obj: Any): Boolean = obj match {
    case that: HashMapBenchmarkData => this.hashCode == that.hashCode && (this.data eq that.data)
    case _ => false
  }

  override def toString: String = s"$hashCode-$data"
}
