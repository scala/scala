package strawman.collection

/**
  * Trait that overrides operations on sequences in order
  * to take advantage of strict builders.
  */
trait StrictOptimizedSeqOps [+A, +CC[_], +C]
  extends SeqOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  override def distinctBy[B](f: A => B): C = {
    val builder = newSpecificBuilder()
    val seen = mutable.HashSet.empty[B]

    for (x <- toIterable) {
      val y = f(x)
      if (!seen.contains(y)) {
        seen += y
        builder += x
      }
    }
    builder.result()
  }
}
