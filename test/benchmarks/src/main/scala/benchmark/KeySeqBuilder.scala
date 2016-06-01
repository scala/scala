package benchmark

/** Builder of a [[KeySeq]]
  * 
  * @tparam K the type of the keys
  */
trait KeySeqBuilder[K] {
  /** Return a [[KeySeq]] having at least the given size. */
  def build(size: Int): KeySeq[K]
}

object KeySeqBuilder {
  /** Builder of a sequence of `Int` keys.
    * Simply maps the sequence index to itself.
    */
  implicit object IntKeySeqBuilder extends KeySeqBuilder[Int] {
    def build(_size: Int) = new KeySeq[Int] {
      def apply(idx: Int) = idx
      def size = _size
    }
  }

  /** Builder of a sequence of `AnyRef` keys. */
  implicit object AnyRefKeySeqBuilder extends KeySeqBuilder[AnyRef] {
    def build(_size: Int) = new KeySeq[AnyRef] {
      private[this] val arr = new Array[AnyRef](size)
      for (i <- 0 until size) arr(i) = new AnyRef()

      def apply(idx: Int) = arr(idx)
      def size = _size
    }
  }
}
