package tastytest

object TestSuperOps extends Suite("TestSuperOps") {

  sealed trait Collection {
    def foo = 23
  }
  object Collection {
    final class Set extends Collection with SuperOps[Set]
  }

  test(assert(new Collection.Set().foo === 23))

}
