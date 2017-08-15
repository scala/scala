package strawman.collection

/**
  * Trait that overrides operations on sequences in order
  * to take advantage of strict builders.
  */
trait StrictOptimizedSeqOps [+A, +CC[_], +C]
  extends SeqOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  override def distinct: C = {
    val builder = newSpecificBuilder()
    val seen = mutable.HashSet.empty[A]

    for (x <- toIterable) {
      if (!seen.contains(x)) {
        seen += x
        builder += x
      }
    }
    builder.result()
  }
}
