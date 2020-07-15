package tastytest

object TestDependentFunctions extends Suite("TestDependentFunctions") {

  class IntEntry(val key: Int) extends DependentFunctions.Entry {
    type Key = Int
  }

  val entry = new IntEntry(3)

  test(assert(DependentFunctions.extractor(entry) === 3))

}
