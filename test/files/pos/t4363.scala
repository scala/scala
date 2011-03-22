object Test {
  trait Suite { def bar() = () }

  () => {
    trait FunkySuite extends Suite { override def bar() = () }
    class MySuite extends FunkySuite { }
  }
}
