package scala.collection.immutable

object HashSetBenchmarkData {
  def apply(hashCode: Int, data: String) = new HashSetBenchmarkData(hashCode, data.intern())
}

class HashSetBenchmarkData private(override val hashCode: Int, val data: String) {
  override def equals(obj: Any): Boolean = obj match {
    case that: HashSetBenchmarkData => this.hashCode == that.hashCode && (this.data eq that.data)
    case _ => false
  }

  override def toString: String = s"$hashCode-$data"
}
