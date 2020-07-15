package tastytest

object DependentFunctions {
  trait Entry { type Key; val key: Key }

  def extractKey(e: Entry): e.Key = e.key
  val extractor: (e: Entry) => e.Key = extractKey
}
