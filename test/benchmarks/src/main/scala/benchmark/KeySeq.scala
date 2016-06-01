package benchmark

/** A sequence of keys.
  * 
  * Tests of maps and sets require a sequence of keys that can be used
  * to add entries and possibly to find them again.
  * This type provides such a sequence.
  * 
  * Note that this needn't be a "sequence" in the full sense of [[collection.Seq]],
  * particularly in that it needn't extend [[PartialFunction]].
  * 
  * @tparam K the type of the keys
  */
trait KeySeq[K] {
  /** Selects a key by its index in the sequence.
    * Repeated calls with the same index return the same key (by reference equality).
    * 
    * @param idx The index to select. Should be non-negative and less than `size`.
    */
  def apply(idx: Int): K

  /** The size of this sequence. */
  def size: Int
}
