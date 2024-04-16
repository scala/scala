//> using options -Xasync -deprecation

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.futures.FutureSpec])

package scala.async {

  import concurrent.{CanAwait, Awaitable}
  import concurrent.duration.Duration
  import java.util.concurrent.{TimeoutException, CountDownLatch, TimeUnit}

  object TestLatch {
    val DefaultTimeout = Duration(5, TimeUnit.SECONDS)

    def apply(count: Int = 1) = new TestLatch(count)
  }


  class TestLatch(count: Int = 1) extends Awaitable[Unit] {
    private var latch = new CountDownLatch(count)

    def countDown() = latch.countDown()

    def isOpen: Boolean = latch.getCount == 0

    def open() = while (!isOpen) countDown()

    def reset() = latch = new CountDownLatch(count)

    @throws(classOf[TimeoutException])
    def ready(atMost: Duration)(implicit permit: CanAwait) = {
      val opened = latch.await(atMost.toNanos, TimeUnit.NANOSECONDS)
      if (!opened) throw new TimeoutException(s"Timeout of ${(atMost.toString)}.")
      this
    }

    @throws(classOf[Exception])
    def result(atMost: Duration)(implicit permit: CanAwait): Unit = {
      ready(atMost)
    }
  }
}

package scala.async.run.futures {

  import java.util.concurrent.ConcurrentHashMap

  import scala.language.postfixOps
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.duration.Duration.Inf
  import scala.collection._
  import scala.runtime.NonLocalReturnControl
  import scala.util.{Try,Success,Failure}
  import scala.reflect.{ClassTag, classTag}

  import scala.async.TestLatch
  import scala.tools.testkit.async.Async.{async, await}

  import org.junit.Test

  class FutureSpec {

    // scala.tools.partest.TestUtil.intercept is not good enough for us
    def intercept[T <: Throwable : ClassTag](body: => Any): T = {
      try {
        body
        throw new Exception(s"Exception of type ${classTag[T]} was not thrown")
      } catch {
        case t: Throwable =>
          if (!classTag[T].runtimeClass.isAssignableFrom(t.getClass)) throw t
          else t.asInstanceOf[T]
      }
    }

    //TODO use normal Assert calls in the tests
    implicit class objectops(obj: Any) {
      def mustBe(other: Any) = assert(obj == other, s"$obj is not $other")

      def mustEqual(other: Any) = mustBe(other)
    }

    /* some utils */

    def testAsync(s: String)(implicit ec: ExecutionContext): Future[String] = s match {
      case "Hello"   => Future { "World" }
      case "Failure" => Future.failed(new RuntimeException("Expected exception; to test fault-tolerance"))
      case "NoReply" => Promise[String]().future
      case x         => throw new MatchError(x)
    }

    val defaultTimeout = 5 seconds

    /* future specification */

      @Test def `A future with custom ExecutionContext should handle Throwables`(): Unit = {
        val ms = new ConcurrentHashMap[Throwable, Unit]
        implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(new java.util.concurrent.ForkJoinPool(), {
          t =>
          ms.put(t, ())
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
        val f3 = async {
          val s = await(f2)
          s.toUpperCase
        }

        f2 foreach { _ => throw new ThrowableTest("dispatcher foreach") }
        f2 onComplete { case Success(_) => throw new ThrowableTest("dispatcher receive"); case _ => }

        latch.open()

        Await.result(f2, defaultTimeout) mustBe ("success")

        f2 foreach { _ => throw new ThrowableTest("current thread foreach") }
        f2 onComplete { case Success(_) => throw new ThrowableTest("current thread receive"); case _ => }

        Await.result(f3, defaultTimeout) mustBe ("SUCCESS")

        val waiting = Future {
          Thread.sleep(1000)
        }
        Await.ready(waiting, 2000 millis)

        ms.size mustBe (4)
      }

      import ExecutionContext.Implicits._

      @Test def `A future with global ExecutionContext should compose with for-comprehensions`(): Unit = {
        import scala.reflect.ClassTag

        def asyncInt(x: Int) = Future { (x * 2).toString }
        val future0 = Future[Any] {
          "five!".length
        }

        val future1 = async {
          val a = await(future0.mapTo[Int])  // returns 5
          val b = await(asyncInt(a))         // returns "10"
          val c = await(asyncInt(7))         // returns "14"
          s"$b-$c"
        }

        val future2 = async {
          val a = await(future0.mapTo[Int])
          val b = await((Future { (a * 2).toString }).mapTo[Int])
          val c = await(Future { (7 * 2).toString })
          s"$b-$c"
        }

        Await.result(future1, defaultTimeout) mustBe ("10-14")
        //assert(checkType(future1, manifest[String]))
        intercept[ClassCastException] { Await.result(future2, defaultTimeout) }
      }

      //TODO this is not yet supported by Async
      @Test def `support pattern matching within a for-comprehension`(): Unit = {
        case class Req[T](req: T)
        case class Res[T](res: T)
        def asyncReq[T](req: Req[T]) = req match {
          case Req(s: String) => Future { Res(s.length) }
          case Req(i: Int)    => Future { Res((i * 2).toString) }
          case _ => ???
        }

        val future1 = for {
          Res(a: Int)    <- asyncReq(Req("Hello"))
          Res(b: String) <- asyncReq(Req(a))
          Res(c: String) <- asyncReq(Req(7))
        } yield s"$b-$c"

        val future2 = for {
          Res(a: Int) <- asyncReq(Req("Hello"))
          Res(b: Int) <- asyncReq(Req(a))
          Res(c: Int) <- asyncReq(Req(7))
        } yield s"$b-$c"

        Await.result(future1, defaultTimeout) mustBe ("10-14")
        intercept[NoSuchElementException] { Await.result(future2, defaultTimeout) }
      }

      @Test def mini(): Unit = {
        val future4 = async {
          await(Future.successful(0)).toString
        }
        Await.result(future4, defaultTimeout)
      }

      @Test def `recover from exceptions`(): Unit = {
        val future1 = Future(5)
        val future2 = async { await(future1) / 0 }
        val future3 = async { await(future2).toString }

        val future1Recovered = future1 recover {
          case e: ArithmeticException => 0
        }
        val future4 = async { await(future1Recovered).toString }

        val future2Recovered = future2 recover {
          case e: ArithmeticException => 0
        }
        val future5 = async { await(future2Recovered).toString }

        val future2Recovered2 = future2 recover {
          case e: MatchError => 0
        }
        val future6 = async { await(future2Recovered2).toString }

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

      @Test def `recoverWith from exceptions`(): Unit = {
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

      @Test def `andThen like a boss`(): Unit = {
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

      @Test def `firstCompletedOf`(): Unit = {
        def futures = Vector.fill[Future[Int]](10) {
          Promise[Int]().future
        } :+ Future.successful[Int](5)

        Await.result(Future.firstCompletedOf(futures), defaultTimeout) mustBe (5)
        Await.result(Future.firstCompletedOf(futures.iterator), defaultTimeout) mustBe (5)
      }

      @Test def `find`(): Unit = {
        val futures = for (i <- 1 to 10) yield Future {
          i
        }

        val result = Future.find[Int](futures)(_ == 3)
        Await.result(result, defaultTimeout) mustBe (Some(3))

        val notFound = Future.find[Int](futures)(_ == 11)
        Await.result(notFound, defaultTimeout) mustBe (None)
      }

      @Test def `zip`(): Unit = {
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

      @Test def `fold`(): Unit = {
        val timeout = 10000 millis
        def async(add: Int, wait: Int) = Future {
          Thread.sleep(wait)
          add
        }

        val futures = (0 to 9) map {
          idx => async(idx, idx * 20)
        }
        // TODO: change to `foldLeft` after support for 2.11 is dropped
        val folded = Future.foldLeft(futures)(0)(_ + _)
        Await.result(folded, timeout) mustBe (45)

        val futuresit = (0 to 9) map {
          idx => async(idx, idx * 20)
        }
        // TODO: change to `foldLeft` after support for 2.11 is dropped
        val foldedit = Future.foldLeft(futures)(0)(_ + _)
        Await.result(foldedit, timeout) mustBe (45)
      }

      @Test def `fold by composing`(): Unit = {
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

      @Test def `fold with an exception`(): Unit = {
        val timeout = 10000 millis
        def async(add: Int, wait: Int) = Future {
          Thread.sleep(wait)
          if (add == 6) throw new IllegalArgumentException("shouldFoldResultsWithException: expected")
          add
        }
        def futures = (0 to 9) map {
          idx => async(idx, idx * 10)
        }
        // TODO: change to `foldLeft` after support for 2.11 is dropped
        val folded = Future.foldLeft(futures)(0)(_ + _)
        intercept[IllegalArgumentException] {
          Await.result(folded, timeout)
        }.getMessage mustBe ("shouldFoldResultsWithException: expected")
      }

      @Test def `fold mutable zeroes safely`(): Unit = {
        import scala.collection.mutable.ArrayBuffer
        def test(testNumber: Int): Unit = {
          val fs = (0 to 1000) map (i => Future(i))
          // TODO: change to `foldLeft` after support for 2.11 is dropped
          val f = Future.foldLeft(fs)(ArrayBuffer.empty[AnyRef]) {
            case (l, i) if i % 2 == 0 => l += i.asInstanceOf[AnyRef]
            case (l, _)               => l
          }
          val result = Await.result(f.mapTo[ArrayBuffer[Int]], 10000 millis).sum

          assert(result == 250500)
        }

        (1 to 100) foreach test //Make sure it tries to provoke the problem
      }

      @Test def `return zero value if folding empty list`(): Unit = {
        // TODO: change to `foldLeft` after support for 2.11 is dropped
        val zero = Future.foldLeft(List[Future[Int]]())(0)(_ + _)
        Await.result(zero, defaultTimeout) mustBe (0)
      }

      @Test def `shouldReduceResults`(): Unit = {
        def async(idx: Int) = Future {
          Thread.sleep(idx * 20)
          idx
        }
        val timeout = 10000 millis

        val futures = (0 to 9) map { async }
        // TODO: change to `reduceLeft` after support for 2.11 is dropped
        val reduced = Future.reduceLeft(futures)(_ + _)
        Await.result(reduced, timeout) mustBe (45)

        val futuresit = (0 to 9) map { async }
        // TODO: change to `reduceLeft` after support for 2.11 is dropped
        val reducedit = Future.reduceLeft(futuresit)(_ + _)
        Await.result(reducedit, timeout) mustBe (45)
      }

      @Test def `shouldReduceResultsWithException`(): Unit = {
        def async(add: Int, wait: Int) = Future {
          Thread.sleep(wait)
          if (add == 6) throw new IllegalArgumentException("shouldFoldResultsWithException: expected")
          else add
        }
        val timeout = 10000 millis
        def futures = (1 to 10) map {
          idx => async(idx, idx * 10)
        }
        // TODO: change to `reduceLeft` after support for 2.11 is dropped
        val failed = Future.reduceLeft(futures)(_ + _)
        intercept[IllegalArgumentException] {
          Await.result(failed, timeout)
        }.getMessage mustBe ("shouldFoldResultsWithException: expected")
      }

      @Test def `shouldReduceThrowNSEEOnEmptyInput`(): Unit = {
        intercept[java.util.NoSuchElementException] {
          // TODO: change to `reduceLeft` after support for 2.11 is dropped
          val emptyreduced = Future.reduceLeft(List[Future[Int]]())(_ + _)
          Await.result(emptyreduced, defaultTimeout)
        }
      }

      @Test def `shouldTraverseFutures`(): Unit = {
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

      @Test def `shouldBlockUntilResult`(): Unit = {
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

      @Test def `run callbacks async`(): Unit = {
        val latch = Vector.fill(10)(new TestLatch)

        val f1 = Future {
          latch(0).open()
          Await.ready(latch(1), TestLatch.DefaultTimeout)
          "Hello"
        }
        val f2 = async {
          val s = await(f1)
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

        val f3 = async {
          val s = await(f1)
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
        val f4 = async {
          val s = await(p1.future)
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

      @Test def `should not deadlock with nested await (ticket 1313)`(): Unit = {
        val simple = async {
          await { Future { } }
          val unit = Future(())
          val umap = unit map { _ => () }
          Await.result(umap, Inf)
        }
        Await.ready(simple, Inf).isCompleted mustBe (true)

        val l1, l2 = new TestLatch
        val complex = async {
          await{ Future { } }
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

      @Test def `should not throw when Await.ready`(): Unit = {
        val expected = try Success(5 / 0) catch { case a: ArithmeticException => Failure(a) }
        val f = async { await(Future(5)) / 0 }
        Await.ready(f, defaultTimeout).value.get.toString mustBe expected.toString
      }
  }

}
