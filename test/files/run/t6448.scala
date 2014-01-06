// Tests to show that various `collect` functions avoid calling
// both `PartialFunction#isDefinedAt` and `PartialFunction#apply`.
//
object Test {
  def f(i: Int) = { println("f(" + i + ")"); true }
  class Counter {
    var count = 0
    def apply(i: Int) = synchronized {count += 1; true}
  }

  def testing(label: String)(body: => Any) {
    println(s"\n=$label=")
    println(body)
  }

  def main(args: Array[String]) {
    testing("List.collect")(List(1, 2) collect { case x if f(x) && x < 2 => x})
    testing("List.collectFirst")(List(1, 2) collectFirst { case x if f(x) && x < 2 => x})
    testing("Option.collect")(Some(1) collect { case x if f(x) && x < 2 => x})
    testing("Option.collect")(Some(2) collect { case x if f(x) && x < 2 => x})
    testing("Stream.collect")((Stream(1, 2).collect { case x if f(x) && x < 2 => x}).toList)
    testing("Stream.collectFirst")(Stream.continually(1) collectFirst { case x if f(x) && x < 2 => x})

    import collection.parallel.ParIterable
    import collection.parallel.immutable.ParVector
    import collection.parallel.mutable.ParArray
    testing("ParVector.collect") {
      val counter = new Counter()
      (ParVector(1, 2) collect { case x if counter(x) && x < 2 => x}, counter.synchronized(counter.count))
    }

    testing("ParArray.collect") {
      val counter = new Counter()
      (ParArray(1, 2) collect { case x if counter(x) && x < 2 => x}, counter.synchronized(counter.count))
    }

    object PendingTests {
      testing("Iterator.collect")((Iterator(1, 2) collect { case x if f(x) && x < 2 => x}).toList)

      testing("List.view.collect")((List(1, 2).view collect { case x if f(x) && x < 2 => x}).force)

      // This would do the trick in Future.collect, but I haven't added this yet as there is a tradeoff
      // with extra allocations to consider.
      //
      // pf.lift(v) match {
      //   case Some(x) => p success x
      //   case None    => fail(v)
      // }
      testing("Future.collect") {
        import concurrent.ExecutionContext.Implicits.global
        import concurrent.Await
        import concurrent.duration.Duration
        val result = concurrent.Future(1) collect { case x if f(x) => x}
        Await.result(result, Duration.Inf)
      }

      // TODO Future.{onSuccess, onFailure, recoverWith, andThen}
    }

  }
}
