


import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.duration.Duration.Inf
import scala.collection._
import scala.runtime.NonLocalReturnControl
import scala.util.{Try,Success,Failure}


class PromiseTests extends MinimalScalaTest {
  import ExecutionContext.Implicits._

  val defaultTimeout = Inf

  /* promise specification */

  "An empty Promise" should {

    "not be completed" in {
      val p = Promise()
      p.future.isCompleted mustBe (false)
      p.isCompleted mustBe (false)
    }

    "have no value" in {
      val p = Promise()
      p.future.value mustBe (None)
      p.isCompleted mustBe (false)
    }

    "return supplied value on timeout" in {
      val failure = Promise.failed[String](new RuntimeException("br0ken")).future
      val otherFailure = Promise.failed[String](new RuntimeException("last")).future
      val empty = Promise[String]().future
      val timedOut = Promise.successful[String]("Timedout").future

      Await.result(failure fallbackTo timedOut, defaultTimeout) mustBe ("Timedout")
      Await.result(timedOut fallbackTo empty, defaultTimeout) mustBe ("Timedout")
      Await.result(otherFailure fallbackTo failure fallbackTo timedOut, defaultTimeout) mustBe ("Timedout")
      intercept[RuntimeException] {
        Await.result(failure fallbackTo otherFailure, defaultTimeout)
      }.getMessage mustBe ("br0ken")
    }

    "be completable with a completed Promise" in {
      {
        val p = Promise[String]()
        p.tryCompleteWith(Promise[String]().success("foo").future)
        Await.result(p.future, defaultTimeout) mustBe ("foo")
      }
      {
        val p = Promise[String]()
        p.completeWith(Promise[String]().success("foo").future)
        Await.result(p.future, defaultTimeout) mustBe ("foo")
      }
      {
        val p = Promise[String]()
        p.tryCompleteWith(Promise[String]().failure(new RuntimeException("br0ken")).future)
        intercept[RuntimeException] {
          Await.result(p.future, defaultTimeout)
        }.getMessage mustBe ("br0ken")
      }
      {
        val p = Promise[String]()
        p.tryCompleteWith(Promise[String]().failure(new RuntimeException("br0ken")).future)
        intercept[RuntimeException] {
          Await.result(p.future, defaultTimeout)
        }.getMessage mustBe ("br0ken")
      }
    }
  }

  "A successful Promise" should {
    "be completed" in {
      val result = "test value"
      val promise = Promise[String]().complete(Success(result))
      promise.isCompleted mustBe (true)
      futureWithResult(_(promise.future, result))
    }

    "not be completable with a completed Promise" in {
      {
        val p = Promise.successful("bar")
        p.tryCompleteWith(Promise[String]().success("foo").future)
        Await.result(p.future, defaultTimeout) mustBe ("bar")
      }
      {
        val p = Promise.successful("bar")
        p.completeWith(Promise[String]().success("foo").future)
        Await.result(p.future, defaultTimeout) mustBe ("bar")
      }
    }
  }

  "A failed Promise" should {
    "be completed" in {
      val message = "Expected Exception"
      val promise = Promise[String]().complete(Failure(new RuntimeException(message)))
      promise.isCompleted mustBe (true)
      futureWithException[RuntimeException](_(promise.future, message))
    }
    "not be completable with a completed Promise" in {
      {
        val p = Promise[String]().failure(new RuntimeException("unbr0ken"))
        p.tryCompleteWith(Promise[String].failure(new Exception("br0ken")).future)
        intercept[RuntimeException] {
          Await.result(p.future, defaultTimeout)
        }.getMessage mustBe ("unbr0ken")
      }
      {
        val p = Promise[String]().failure(new RuntimeException("unbr0ken"))
        p.completeWith(Promise[String]().failure(new Exception("br0ken")).future)
        intercept[RuntimeException] {
          Await.result(p.future, defaultTimeout)
        }.getMessage mustBe ("unbr0ken")
      }
    }
  }

  "An interrupted Promise" should {
    val message = "Boxed InterruptedException"
    val future = Promise[String]().complete(Failure(new InterruptedException(message))).future
    futureWithException[ExecutionException](_(future, message))
  }

  "A NonLocalReturnControl failed Promise" should {
    val result = "test value"
    val future = Promise[String]().complete(Failure(new NonLocalReturnControl[String]("test", result))).future
    futureWithResult(_(future, result))
  }

  def futureWithResult(f: ((Future[Any], Any) => Unit) => Unit) {

    "be completed" in { f((future, _) => future.isCompleted mustBe (true)) }

    "contain a value" in { f((future, result) => future.value mustBe (Some(Success(result)))) }

    "return when ready with 'Await.ready'" in { f((future, result) => Await.ready(future, defaultTimeout).isCompleted mustBe (true)) }

    "return result with 'Await.result'" in { f((future, result) => Await.result(future, defaultTimeout) mustBe (result)) }

    "not timeout" in { f((future, _) => Await.ready(future, 0 millis)) }

    "filter result" in {
      f {
        (future, result) =>
        Await.result((future filter (_ => true)), defaultTimeout) mustBe (result)
        intercept[NoSuchElementException] {
          Await.result((future filter (_ => false)), defaultTimeout)
        }
      }
    }

    "transform result with map" in { f((future, result) => Await.result((future map (_.toString.length)), defaultTimeout) mustBe (result.toString.length)) }

    "compose result with flatMap" in {
      f { (future, result) =>
        val r = for (r <- future; p <- Promise.successful("foo").future) yield r.toString + p
        Await.result(r, defaultTimeout) mustBe (result.toString + "foo")
      }
    }

    "perform action with foreach" in {
      f {
        (future, result) =>
        val p = Promise[Any]()
        future foreach p.success
        Await.result(p.future, defaultTimeout) mustBe (result)
      }
    }

    "zip properly" in {
      f {
        (future, result) =>
        Await.result(future zip Promise.successful("foo").future, defaultTimeout) mustBe ((result, "foo"))
        intercept[RuntimeException] {
          Await.result(future zip Promise.failed(new RuntimeException("ohnoes")).future, defaultTimeout)
        }.getMessage mustBe ("ohnoes")
      }
    }

    "not recover from exception" in { f((future, result) => Await.result(future.recover({ case _ => "pigdog" }), defaultTimeout) mustBe (result)) }

    "perform action on result" in {
      f {
        (future, result) =>
        val p = Promise[Any]()
        future.onSuccess { case x => p.success(x) }
        Await.result(p.future, defaultTimeout) mustBe (result)
      }
    }

    "not project a failure" in {
      f {
        (future, result) =>
          intercept[NoSuchElementException] {
            Await.result(future.failed, defaultTimeout)
          }.getMessage mustBe ("Future.failed not completed with a throwable.")
      }
    }

    "cast using mapTo" in {
      f {
        (future, result) =>
        Await.result(future.mapTo[Boolean].recover({ case _: ClassCastException ⇒ false }), defaultTimeout) mustBe (false)
      }
    }

  }

  def futureWithException[E <: Throwable: Manifest](f: ((Future[Any], String) => Unit) => Unit) {

    "be completed" in {
      f((future, _) => future.isCompleted mustBe (true))
    }

    "contain a value" in {
      f((future, message) => {
        future.value.get.failed.get.getMessage mustBe (message)
      })
    }

    "throw not throw exception with 'Await.ready'" in {
      f {
        (future, message) => Await.ready(future, defaultTimeout).isCompleted mustBe (true)
      }
    }

    "throw exception with 'Await.result'" in {
      f {
        (future, message) =>
        intercept[E] {
          Await.result(future, defaultTimeout)
        }.getMessage mustBe (message)
      }
    }

    "retain exception with filter" in {
      f {
        (future, message) =>
        intercept[E] { Await.result(future filter (_ => true), defaultTimeout) }.getMessage mustBe (message)
        intercept[E] { Await.result(future filter (_ => false), defaultTimeout) }.getMessage mustBe (message)
      }
    }

    "retain exception with map" in {
      f {
        (future, message) =>
        intercept[E] { Await.result(future map (_.toString.length), defaultTimeout) }.getMessage mustBe (message)
      }
    }

    "retain exception with flatMap" in {
      f {
        (future, message) =>
        intercept[E] { Await.result(future flatMap (_ => Promise.successful("foo").future), defaultTimeout) }.getMessage mustBe (message)
      }
    }

    "zip properly" in {
      f {
        (future, message) =>
        intercept[E] {
          Await.result(future zip Promise.successful("foo").future, defaultTimeout)
        }.getMessage mustBe (message)
      }
    }

    "recover from exception" in {
      f {
        (future, message) =>
        Await.result(future.recover({ case e if e.getMessage == message ⇒ "pigdog" }), defaultTimeout) mustBe ("pigdog")
      }
    }

    "project a failure" in {
      f((future, message) => Await.result(future.failed, defaultTimeout).getMessage mustBe (message))
    }

    "perform action on exception" in {
      f {
        (future, message) =>
        val p = Promise[Any]()
        future.onFailure { case _ => p.success(message) }
        Await.result(p.future, defaultTimeout) mustBe (message)
      }
    }

    "always cast successfully using mapTo" in {
      f {
        (future, message) =>
          intercept[E] { Await.result(future.mapTo[java.lang.Thread], defaultTimeout) }.getMessage mustBe (message)
      }
    }
  }
}







