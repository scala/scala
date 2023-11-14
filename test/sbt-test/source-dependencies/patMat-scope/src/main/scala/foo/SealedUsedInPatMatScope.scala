package foo

object SealedUsedInPatMatScope {
  def foo(a: Sealed) = a match {
    case SealedChild2 =>
      2
    case _ =>
      3
  }
}