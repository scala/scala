class D extends C {
  // mention abc name to check if local inheritance dependencies are _not_ included in member reference
  // extension of inheritance invalidation
  def bar(abc: Int): Int = abc
}
