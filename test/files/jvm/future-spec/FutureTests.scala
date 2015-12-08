import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.duration.Duration.Inf
import scala.collection._
import scala.runtime.NonLocalReturnControl
import scala.util.{Try,Success,Failure}



class FutureTests extends MinimalScalaTest {

  /* some utils */

  def testAsync(s: String)(implicit ec: ExecutionContext): Future[String] = s match {
    case "Hello"   => Future { "World" }
    case "Failure" => Future.failed(new RuntimeException("Expected exception; to test fault-tolerance"))
    case "NoReply" => Promise[String]().future
  }

  def fail(msg: String): Nothing = throw new AssertionError(msg)

  def ECNotUsed[T](f: ExecutionContext => T): T = {
    val p = Promise[Runnable]()
    val unusedEC: ExecutionContext = new ExecutionContext {
      def execute(r: Runnable) = p.success(r)
      def reportFailure(t: Throwable): Unit = p.failure(t)
    }
    val t = f(unusedEC)
    assert(p.future.value == None, "Future executed logic!")
    t
  }

  val defaultTimeout = 5 seconds

  /* future specification */

  "A future with custom ExecutionContext" should {
    "shouldHandleThrowables" in {
      val ms = new mutable.HashSet[Throwable] with mutable.SynchronizedSet[Throwable]
      implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(new java.util.concurrent.ForkJoinPool(), {
        t =>
        ms += t
      })

      class ThrowableTest(m: String) extends Throwable(m)

      val f1 = Future[Any] {
        throw new ThrowableTest("test")
      }

      intercept[ThrowableTest] {
        Await.result(f1, defaultTimeout)
      }

      val latch = new TestLatch
      val f2 = Future {
        Await.ready(latch, 5 seconds)
        "success"
      }
      val f3 = f2 map { s => s.toUpperCase }

      f2 foreach { _ => throw new ThrowableTest("dispatcher foreach") }
      f2 onSuccess { case _ => throw new ThrowableTest("dispatcher receive") }

      latch.open()

      Await.result(f2, defaultTimeout) mustBe ("success")

      f2 foreach { _ => throw new ThrowableTest("current thread foreach") }
      f2 onSuccess { case _ => throw new ThrowableTest("current thread receive") }

      Await.result(f3, defaultTimeout) mustBe ("SUCCESS")

      val waiting = Future {
        Thread.sleep(1000)
      }
      Await.ready(waiting, 2000 millis)

      ms.size mustBe (4)
      //FIXME should check
    }
  }

  "Futures" should {
    "have proper toString representations" in {
      import ExecutionContext.Implicits.global
      val s = 5
      val f = new Exception("foo")
      val t = Try(throw f)

      val expectFailureString = "Future(Failure("+f+"))"
      val expectSuccessString = "Future(Success(5))"
      val expectNotCompleteString = "Future(<not completed>)"

      Future.successful(s).toString mustBe expectSuccessString
      Future.failed(f).toString mustBe expectFailureString
      Future.fromTry(t).toString mustBe expectFailureString
      val p = Promise[Int]()
      p.toString mustBe expectNotCompleteString
      Promise[Int]().success(s).toString mustBe expectSuccessString
      Promise[Int]().failure(f).toString mustBe expectFailureString
      Await.ready(Future { throw f }, 2000 millis).toString mustBe expectFailureString
      Await.ready(Future { s }, 2000 millis).toString mustBe expectSuccessString

      Future.never.toString mustBe "Future(<never>)"
      Future.unit.toString mustBe "Future(Success(()))"
    }

    "have proper const representation for success" in {
      val s = "foo"
      val f = Future.successful(s)

      ECNotUsed(ec => f.onFailure({ case _ => fail("onFailure should not have been called") })(ec))
      assert( ECNotUsed(ec => f.recover({ case _ => fail("recover should not have been called")})(ec)) eq f)
      assert( ECNotUsed(ec => f.recoverWith({ case _ => fail("flatMap should not have been called")})(ec)) eq f)
      assert(f.fallbackTo(f) eq f, "Future.fallbackTo must be the same instance as Future.fallbackTo")
    }

    "have proper const representation for failure" in {
      val e = new Exception("foo")
      val f = Future.failed[Future[String]](e)

      assert(f.mapTo[String] eq f, "Future.mapTo must be the same instance as Future.mapTo")
      assert(f.zip(f) eq f, "Future.zip must be the same instance as Future.zip")
      assert(f.flatten eq f, "Future.flatten must be the same instance as Future.flatten")
      assert(f.failed eq f, "Future.failed must be the same instance as Future.failed")

              ECNotUsed(ec => f.foreach(_ => fail("foreach should not have been called"))(ec))
              ECNotUsed(ec => f.onSuccess({ case _ => fail("onSuccess should not have been called") })(ec))
      assert( ECNotUsed(ec => f.map(_ => fail("map should not have been called"))(ec)) eq f)
      assert( ECNotUsed(ec => f.flatMap(_ => fail("flatMap should not have been called"))(ec)) eq f)
      assert( ECNotUsed(ec => f.filter(_ => fail("filter should not have been called"))(ec)) eq f)
      assert( ECNotUsed(ec => f.collect({ case _ => fail("collect should not have been called")})(ec)) eq f)
      assert( ECNotUsed(ec => f.zipWith(f)({ (_,_) => fail("zipWith should not have been called")})(ec)) eq f)
    }
  }

  "The Future companion object" should {
    "call ExecutionContext.prepare on apply" in {
      val p = Promise[Boolean]()
      val ec = new ExecutionContext {
        val delegate = ExecutionContext.global
        override def prepare(): ExecutionContext = {
          p.success(true)
          delegate.prepare
        }
        override def execute(r: Runnable) = delegate.execute(r)
        override def reportFailure(t: Throwable): Unit = delegate.reportFailure(t)
      }

      val f = Future("foo")(ec)
      Await.result(f, defaultTimeout) mustBe ("foo")
      Await.result(p.future, defaultTimeout) mustBe (true)
    }

    "have a unit member representing an already completed Future containing Unit" in {
      assert(Future.unit ne null, "Future.unit must not be null")
      assert(Future.unit eq Future.unit, "Future.unit must be the same instance as Future.unit")
      assert(Future.unit.isCompleted, "Future.unit must already be completed")
      assert(Future.unit.value.get == Success(()), "Future.unit must contain a Success(())")
    }

    "have a never member representing a never completed Future of Nothing" in {

      val test: Future[Nothing] = Future.never

      //Verify stable identifier
      test match {
        case Future.`never` =>
        case _ => fail("Future.never did not match Future.`never`")
      }

      assert(test eq Future.never, "Future.never must be the same instance as Future.never")
      assert(test ne null, "Future.never must not be null")
      assert(!test.isCompleted && test.value.isEmpty, "Future.never must never be completed")
      assert(test.failed eq test)
      assert(test.asInstanceOf[Future[Future[Nothing]]].flatten eq test)
      assert(test.zip(test) eq test)
      assert(test.fallbackTo(test) eq test)
      assert(test.mapTo[String] eq test)

      ECNotUsed(ec => test.foreach(_ => fail("foreach should not have been called"))(ec))
      ECNotUsed(ec => test.onSuccess({ case _ => fail("onSuccess should not have been called") })(ec))
      ECNotUsed(ec => test.onFailure({ case _ => fail("onFailure should not have been called") })(ec))
      ECNotUsed(ec => test.onComplete({ case _ => fail("onComplete should not have been called") })(ec))
      ECNotUsed(ec => test.transform(identity, identity)(ec) eq test)
      ECNotUsed(ec => test.transform(identity)(ec) eq test)
      ECNotUsed(ec => test.transformWith(_ => fail("transformWith should not have been called"))(ec) eq test)
      ECNotUsed(ec => test.map(identity)(ec) eq test)
      ECNotUsed(ec => test.flatMap(_ => fail("flatMap should not have been called"))(ec) eq test)
      ECNotUsed(ec => test.filter(_ => fail("filter should not have been called"))(ec) eq test)
      ECNotUsed(ec => test.collect({ case _ => fail("collect should not have been called")})(ec) eq test)
      ECNotUsed(ec => test.recover({ case _ => fail("recover should not have been called")})(ec) eq test)
      ECNotUsed(ec => test.recoverWith({ case _ => fail("recoverWith should not have been called")})(ec) eq test)
      ECNotUsed(ec => test.andThen({ case _ => fail("andThen should not have been called")})(ec) eq test)
      ECNotUsed(ec => test.zipWith(test)({ (_,_) => fail("zipWith should not have been called")})(ec) eq test)
    }
  }

  "The default ExecutionContext" should {
    "report uncaught exceptions" in {
      val p = Promise[Throwable]()
      val logThrowable: Throwable => Unit = p.trySuccess(_)
      val ec: ExecutionContext = ExecutionContext.fromExecutor(null, logThrowable)

      val t = new InterruptedException()
      val f = Future(throw t)(ec)
      Await.result(p.future, 2.seconds) mustBe t
    }
  }

  "A future with global ExecutionContext" should {
    import ExecutionContext.Implicits._

    "compose with for-comprehensions" in {
      def async(x: Int) = Future { (x * 2).toString }
      val future0 = Future[Any] {
        "five!".length
      }

      val future1 = for {
        a <- future0.mapTo[Int]  // returns 5
        b <- async(a)            // returns "10"
        c <- async(7)            // returns "14"
      } yield b + "-" + c

      val future2 = for {
        a <- future0.mapTo[Int]
        b <- (Future { (a * 2).toString }).mapTo[Int]
        c <- Future { (7 * 2).toString }
      } yield b + "-" + c

      Await.result(future1, defaultTimeout) mustBe ("10-14")
      assert(checkType(future1, manifest[String]))
      intercept[ClassCastException] { Await.result(future2, defaultTimeout) }
    }

    "support pattern matching within a for-comprehension" in {
      case class Req[T](req: T)
      case class Res[T](res: T)
      def async[T](req: Req[T]) = req match {
        case Req(s: String) => Future { Res(s.length) }
        case Req(i: Int)    => Future { Res((i * 2).toString) }
      }

      val future1 = for {
        Res(a: Int) <- async(Req("Hello"))
        Res(b: String) <- async(Req(a))
        Res(c: String) <- async(Req(7))
      } yield b + "-" + c

      val future2 = for {
        Res(a: Int) <- async(Req("Hello"))
        Res(b: Int) <- async(Req(a))
        Res(c: Int) <- async(Req(7))
      } yield b + "-" + c

      Await.result(future1, defaultTimeout) mustBe ("10-14")
      intercept[NoSuchElementException] { Await.result(future2, defaultTimeout) }
    }

    "recover from exceptions" in {
      val future1 = Future(5)
      val future2 = future1 map (_ / 0)
      val future3 = future2 map (_.toString)

      val future4 = future1 recover {
        case e: ArithmeticException => 0
      } map (_.toString)

      val future5 = future2 recover {
        case e: ArithmeticException => 0
      } map (_.toString)

      val future6 = future2 recover {
        case e: MatchError => 0
      } map (_.toString)

      val future7 = future3 recover {
        case e: ArithmeticException => "You got ERROR"
      }

      val future8 = testAsync("Failure")
      val future9 = testAsync("Failure") recover {
        case e: RuntimeException => "FAIL!"
      }
      val future10 = testAsync("Hello") recover {
        case e: RuntimeException => "FAIL!"
      }
      val future11 = testAsync("Failure") recover {
        case _ => "Oops!"
      }

      Await.result(future1, defaultTimeout) mustBe (5)
      intercept[ArithmeticException] { Await.result(future2, defaultTimeout) }
      intercept[ArithmeticException] { Await.result(future3, defaultTimeout) }
      Await.result(future4, defaultTimeout) mustBe ("5")
      Await.result(future5, defaultTimeout) mustBe ("0")
      intercept[ArithmeticException] { Await.result(future6, defaultTimeout) }
      Await.result(future7, defaultTimeout) mustBe ("You got ERROR")
      intercept[RuntimeException] { Await.result(future8, defaultTimeout) }
      Await.result(future9, defaultTimeout) mustBe ("FAIL!")
      Await.result(future10, defaultTimeout) mustBe ("World")
      Await.result(future11, defaultTimeout) mustBe ("Oops!")
    }

    "recoverWith from exceptions" in {
      val o = new IllegalStateException("original")
      val r = new IllegalStateException("recovered")

      intercept[IllegalStateException] {
        val failed = Future.failed[String](o) recoverWith {
          case _ if false == true => Future.successful("yay!")
        }
        Await.result(failed, defaultTimeout)
      } mustBe (o)

      val recovered = Future.failed[String](o) recoverWith {
        case _ => Future.successful("yay!")
      }
      Await.result(recovered, defaultTimeout) mustBe ("yay!")

      intercept[IllegalStateException] {
        val refailed = Future.failed[String](o) recoverWith {
          case _ => Future.failed[String](r)
        }
        Await.result(refailed, defaultTimeout)
      } mustBe (r)
    }

    "transform results to results" in {
      val f1 = Future.successful("foo").transform(_.map(_.toUpperCase))
      val f2 = Future("bar").transform(_.map(_.toUpperCase))
      Await.result(f1, defaultTimeout) mustBe "FOO"
      Await.result(f2, defaultTimeout) mustBe "BAR"
    }

    "transform failures to failures" in {
      val initial = new Exception("Initial")
      val expected1 = new Exception("Expected1")
      val expected2 = new Exception("Expected2")
      val f1 = Future(throw initial) transform {
        case Failure(`initial`) => Failure(expected1)
        case x => x
      }
      val f2 = Future.failed(initial) transform {
        case Failure(`initial`) => Failure(expected2)
        case x => x
      }

      intercept[Exception] { Await.result(f1, defaultTimeout) } mustBe expected1
      intercept[Exception] { Await.result(f2, defaultTimeout) } mustBe expected2
    }

    "transform failures to results" in {
      val initial1 = new Exception("Initial1")
      val initial2 = new Exception("Initial2")
      val f1 = Future.failed[String](initial1) transform {
        case Failure(`initial1`) => Success("foo")
        case x => x
      }
      val f2 = Future[String](throw initial2) transform {
        case Failure(`initial2`) => Success("bar")
        case x => x
      }
      Await.result(f1, defaultTimeout) mustBe "foo"
      Await.result(f2, defaultTimeout) mustBe "bar"
    }

    "transform results to failures" in {
      val expected1 = new Exception("Expected1")
      val expected2 = new Exception("Expected2")
      val expected3 = new Exception("Expected3")
      val f1 = Future.successful("foo") transform {
        case Success("foo") => Failure(expected1)
        case x => x
      }
      val f2 = Future("bar") transform {
        case Success("bar") => Failure(expected2)
        case x => x
      }
      val f3 = Future("bar") transform {
        case Success("bar") => throw expected3
        case x => x
      }
      intercept[Exception] { Await.result(f1, defaultTimeout) } mustBe expected1
      intercept[Exception] { Await.result(f2, defaultTimeout) } mustBe expected2
      intercept[Exception] { Await.result(f3, defaultTimeout) } mustBe expected3
    }

    "transformWith results" in {
      val f1 = Future.successful("foo").transformWith {
        case Success(r) => Future(r.toUpperCase)
        case f @ Failure(_) => Future.fromTry(f)
      }
      val f2 = Future("bar").transformWith {
        case Success(r) => Future(r.toUpperCase)
        case f @ Failure(_) => Future.fromTry(f)
      }
      Await.result(f1, defaultTimeout) mustBe "FOO"
      Await.result(f2, defaultTimeout) mustBe "BAR"
    }

    "transformWith failures" in {
      val initial = new Exception("Initial")
      val expected1 = new Exception("Expected1")
      val expected2 = new Exception("Expected2")
      val expected3 = new Exception("Expected3")

      val f1 = Future[Int](throw initial).transformWith {
        case Failure(`initial`) => Future failed expected1
        case x => Future fromTry x
      }
      val f2 = Future.failed[Int](initial).transformWith {
        case Failure(`initial`) => Future failed expected2
        case x => Future fromTry x
      }
      val f3 = Future[Int](throw initial).transformWith {
        case Failure(`initial`) => throw expected3
        case x => Future fromTry x
      }

      intercept[Exception] { Await.result(f1, defaultTimeout) } mustBe expected1
      intercept[Exception] { Await.result(f2, defaultTimeout) } mustBe expected2
      intercept[Exception] { Await.result(f3, defaultTimeout) } mustBe expected3
    }

    "transformWith failures to future success" in {
      val initial = new Exception("Initial")
      val f1 = Future.failed[String](initial).transformWith {
        case Failure(`initial`) => Future("FOO")
        case _ => Future failed initial
      }
      val f2 = Future[String](throw initial).transformWith {
        case Failure(`initial`) => Future("BAR")
        case _ => Future failed initial
      }
      Await.result(f1, defaultTimeout) mustBe "FOO"
      Await.result(f2, defaultTimeout) mustBe "BAR"
    }

    "transformWith results to future failures" in {
      val initial = new Exception("Initial")
      val expected1 = new Exception("Expected1")
      val expected2 = new Exception("Expected2")
      val expected3 = new Exception("Expected3")

      val f1 = Future[String]("FOO") transformWith {
        case Success("FOO") => Future failed expected1
        case _ => Future successful "FOO"
      }
      val f2 = Future.successful("FOO") transformWith {
        case Success("FOO") => Future failed expected2
        case _ => Future successful "FOO"
      }
      val f3 = Future.successful("FOO") transformWith {
        case Success("FOO") => throw expected3
        case _ => Future successful "FOO"
      }


      intercept[Exception] { Await.result(f1, defaultTimeout) } mustBe expected1
      intercept[Exception] { Await.result(f2, defaultTimeout) } mustBe expected2
      intercept[Exception] { Await.result(f3, defaultTimeout) } mustBe expected3
    }

    "andThen like a boss" in {
      val q = new java.util.concurrent.LinkedBlockingQueue[Int]
      for (i <- 1 to 1000) {
        val chained = Future {
          q.add(1); 3
        } andThen {
          case _ => q.add(2)
        } andThen {
          case Success(0) => q.add(Int.MaxValue)
        } andThen {
          case _ => q.add(3);
        }
        Await.result(chained, defaultTimeout) mustBe (3)
        q.poll() mustBe (1)
        q.poll() mustBe (2)
        q.poll() mustBe (3)
        q.clear()
      }
    }

    "firstCompletedOf" in {
      def futures = Vector.fill[Future[Int]](10) {
        Promise[Int]().future
      } :+ Future.successful[Int](5)

      Await.result(Future.firstCompletedOf(futures), defaultTimeout) mustBe (5)
      Await.result(Future.firstCompletedOf(futures.iterator), defaultTimeout) mustBe (5)
    }

    "find" in {
      val futures = for (i <- 1 to 10) yield Future {
        i
      }

      val result = Future.find[Int](futures)(_ == 3)
      Await.result(result, defaultTimeout) mustBe (Some(3))

      val notFound = Future.find[Int](futures.iterator)(_ == 11)
      Await.result(notFound, defaultTimeout) mustBe (None)
    }

    "zip" in {
      val timeout = 10000 millis
      val f = new IllegalStateException("test")
      intercept[IllegalStateException] {
        val failed = Future.failed[String](f) zip Future.successful("foo")
        Await.result(failed, timeout)
      } mustBe (f)

      intercept[IllegalStateException] {
        val failed = Future.successful("foo") zip Future.failed[String](f)
        Await.result(failed, timeout)
      } mustBe (f)

      intercept[IllegalStateException] {
        val failed = Future.failed[String](f) zip Future.failed[String](f)
        Await.result(failed, timeout)
      } mustBe (f)

      val successful = Future.successful("foo") zip Future.successful("foo")
      Await.result(successful, timeout) mustBe (("foo", "foo"))
    }

    "zipWith" in {
      val timeout = 10000 millis
      val f = new IllegalStateException("test")
      intercept[IllegalStateException] {
        val failed = Future.failed[String](f).zipWith(Future.successful("foo")) { _ -> _ }
        Await.result(failed, timeout)
      } mustBe (f)

      intercept[IllegalStateException] {
        val failed = Future.successful("foo").zipWith(Future.failed[String](f)) { _ -> _ }
        Await.result(failed, timeout)
      } mustBe (f)

      intercept[IllegalStateException] {
        val failed = Future.failed[String](f).zipWith(Future.failed[String](f)) { _ -> _ }
        Await.result(failed, timeout)
      } mustBe (f)

      val successful = Future.successful("foo").zipWith(Future.successful("foo")) { _ -> _ }
      Await.result(successful, timeout) mustBe (("foo", "foo"))

      val failure = Future.successful("foo").zipWith(Future.successful("foo")) { (_,_) => throw f }
      intercept[IllegalStateException] {
        Await.result(failure, timeout)
      } mustBe (f)
    }

    "fold" in {
      val timeout = 10000 millis
      def async(add: Int, wait: Int) = Future {
        Thread.sleep(wait)
        add
      }

      val futures = (0 to 9) map {
        idx => async(idx, idx * 20)
      }
      val folded = Future.fold(futures)(0)(_ + _)
      Await.result(folded, timeout) mustBe (45)

      val futuresit = (0 to 9) map {
        idx => async(idx, idx * 20)
      }
      val foldedit = Future.fold(futures)(0)(_ + _)
      Await.result(foldedit, timeout) mustBe (45)
    }

    "fold by composing" in {
      val timeout = 10000 millis
      def async(add: Int, wait: Int) = Future {
        Thread.sleep(wait)
        add
      }
      def futures = (0 to 9) map {
        idx => async(idx, idx * 20)
      }
      val folded = futures.foldLeft(Future(0)) {
        case (fr, fa) => for (r <- fr; a <- fa) yield (r + a)
      }
      Await.result(folded, timeout) mustBe (45)
    }

    "fold with an exception" in {
      val timeout = 10000 millis
      def async(add: Int, wait: Int) = Future {
        Thread.sleep(wait)
        if (add == 6) throw new IllegalArgumentException("shouldFoldResultsWithException: expected")
        add
      }
      def futures = (0 to 9) map {
        idx => async(idx, idx * 10)
      }
      val folded = Future.fold(futures)(0)(_ + _)
      intercept[IllegalArgumentException] {
        Await.result(folded, timeout)
      }.getMessage mustBe ("shouldFoldResultsWithException: expected")
    }

    "fold mutable zeroes safely" in {
      import scala.collection.mutable.ArrayBuffer
      def test(testNumber: Int) {
        val fs = (0 to 1000) map (i => Future(i))
        val f = Future.fold(fs)(ArrayBuffer.empty[AnyRef]) {
          case (l, i) if i % 2 == 0 => l += i.asInstanceOf[AnyRef]
          case (l, _)               => l
        }
        val result = Await.result(f.mapTo[ArrayBuffer[Int]], 10000 millis).sum

        assert(result == 250500)
      }

      (1 to 100) foreach test //Make sure it tries to provoke the problem
    }

    "return zero value if folding empty list" in {
      val zero = Future.fold(List[Future[Int]]())(0)(_ + _)
      Await.result(zero, defaultTimeout) mustBe (0)
    }

    "shouldReduceResults" in {
      def async(idx: Int) = Future {
        Thread.sleep(idx * 20)
        idx
      }
      val timeout = 10000 millis

      val futures = (0 to 9) map { async }
      val reduced = Future.reduce(futures)(_ + _)
      Await.result(reduced, timeout) mustBe (45)

      val futuresit = (0 to 9) map { async }
      val reducedit = Future.reduce(futuresit)(_ + _)
      Await.result(reducedit, timeout) mustBe (45)
    }

    "shouldReduceResultsWithException" in {
      def async(add: Int, wait: Int) = Future {
        Thread.sleep(wait)
        if (add == 6) throw new IllegalArgumentException("shouldFoldResultsWithException: expected")
        else add
      }
      val timeout = 10000 millis
      def futures = (1 to 10) map {
        idx => async(idx, idx * 10)
      }
      val failed = Future.reduce(futures)(_ + _)
      intercept[IllegalArgumentException] {
        Await.result(failed, timeout)
      }.getMessage mustBe ("shouldFoldResultsWithException: expected")
    }

    "shouldReduceThrowNSEEOnEmptyInput" in {
      intercept[java.util.NoSuchElementException] {
        val emptyreduced = Future.reduce(List[Future[Int]]())(_ + _)
        Await.result(emptyreduced, defaultTimeout)
      }
    }

    "shouldTraverseFutures" in {
      object counter {
        var count = -1
        def incAndGet() = counter.synchronized {
          count += 2
          count
        }
      }

      val oddFutures = List.fill(100)(Future { counter.incAndGet() }).iterator
      val traversed = Future.sequence(oddFutures)
      Await.result(traversed, defaultTimeout).sum mustBe (10000)

      val list = (1 to 100).toList
      val traversedList = Future.traverse(list)(x => Future(x * 2 - 1))
      Await.result(traversedList, defaultTimeout).sum mustBe (10000)

      val iterator = (1 to 100).toList.iterator
      val traversedIterator = Future.traverse(iterator)(x => Future(x * 2 - 1))
      Await.result(traversedIterator, defaultTimeout).sum mustBe (10000)
    }

    "shouldBlockUntilResult" in {
      val latch = new TestLatch

      val f = Future {
        Await.ready(latch, 5 seconds)
        5
      }
      val f2 = Future {
        val res = Await.result(f, Inf)
        res + 9
      }

      intercept[TimeoutException] {
        Await.ready(f2, 100 millis)
      }

      latch.open()

      Await.result(f2, defaultTimeout) mustBe (14)

      val f3 = Future {
        Thread.sleep(100)
        5
      }

      intercept[TimeoutException] {
        Await.ready(f3, 0 millis)
      }
    }

    "run callbacks async" in {
      val latch = Vector.fill(10)(new TestLatch)

      val f1 = Future {
        latch(0).open()
        Await.ready(latch(1), TestLatch.DefaultTimeout)
        "Hello"
      }
      val f2 = f1 map {
        s =>
        latch(2).open()
        Await.ready(latch(3), TestLatch.DefaultTimeout)
        s.length
      }
      for (_ <- f2) latch(4).open()

      Await.ready(latch(0), TestLatch.DefaultTimeout)

      f1.isCompleted mustBe (false)
      f2.isCompleted mustBe (false)

      latch(1).open()
      Await.ready(latch(2), TestLatch.DefaultTimeout)

      f1.isCompleted mustBe (true)
      f2.isCompleted mustBe (false)

      val f3 = f1 map {
        s =>
        latch(5).open()
        Await.ready(latch(6), TestLatch.DefaultTimeout)
        s.length * 2
      }
      for (_ <- f3) latch(3).open()

      Await.ready(latch(5), TestLatch.DefaultTimeout)

      f3.isCompleted mustBe (false)

      latch(6).open()
      Await.ready(latch(4), TestLatch.DefaultTimeout)

      f2.isCompleted mustBe (true)
      f3.isCompleted mustBe (true)

      val p1 = Promise[String]()
      val f4 = p1.future map {
        s =>
        latch(7).open()
        Await.ready(latch(8), TestLatch.DefaultTimeout)
        s.length
      }
      for (_ <- f4) latch(9).open()

      p1.future.isCompleted mustBe (false)
      f4.isCompleted mustBe (false)

      p1 complete Success("Hello")

      Await.ready(latch(7), TestLatch.DefaultTimeout)

      p1.future.isCompleted mustBe (true)
      f4.isCompleted mustBe (false)

      latch(8).open()
      Await.ready(latch(9), TestLatch.DefaultTimeout)

      Await.ready(f4, defaultTimeout).isCompleted mustBe (true)
    }

    "should not deadlock with nested await (ticket 1313)" in {
      val simple = Future(()) map {
        _ =>
        val unit = Future(())
        val umap = unit map { _ => () }
        Await.result(umap, Inf)
      }
      Await.ready(simple, Inf).isCompleted mustBe (true)

      val l1, l2 = new TestLatch
      val complex = Future(()) map {
        _ =>
        blocking {
          val nested = Future(())
          for (_ <- nested) l1.open()
          Await.ready(l1, TestLatch.DefaultTimeout) // make sure nested is completed
          for (_ <- nested) l2.open()
          Await.ready(l2, TestLatch.DefaultTimeout)
        }
      }
      Await.ready(complex, defaultTimeout).isCompleted mustBe (true)
    }

    "should not throw when Await.ready" in {
      val expected = try Success(5 / 0) catch { case a: ArithmeticException => Failure(a) }
      val f = Future(5).map(_ / 0)
      Await.ready(f, defaultTimeout).value.get.toString mustBe expected.toString
    }

  }

}
