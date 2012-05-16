


import scala.concurrent._
import scala.concurrent.util.duration._
import scala.concurrent.util.Duration.Inf
import scala.collection._
import scala.runtime.NonLocalReturnControl



object PromiseTests extends MinimalScalaTest {
  
  val defaultTimeout = Inf
  
  /* promise specification */
  
  "An empty Promise" should {
    
    "not be completed" in {
      val p = Promise()
      p.future.isCompleted mustBe (false)
    }
    
    "have no value" in {
      val p = Promise()
      p.future.value mustBe (None)
    }
    
    "return supplied value on timeout" in {
      val failure = Promise.failed[String](new RuntimeException("br0ken")).future
      val otherFailure = Promise.failed[String](new RuntimeException("last")).future
      val empty = Promise[String]().future
      val timedOut = Promise.successful[String]("Timedout").future
      
      Await.result(failure fallbackTo timedOut, defaultTimeout) mustBe ("Timedout")
      Await.result(timedOut fallbackTo empty, defaultTimeout) mustBe ("Timedout")
      Await.result(failure fallbackTo failure fallbackTo timedOut, defaultTimeout) mustBe ("Timedout")
      intercept[RuntimeException] {
        Await.result(failure fallbackTo otherFailure, defaultTimeout)
      }.getMessage mustBe ("last")
    }
    
  }
  
  "A successful Promise" should {
    val result = "test value"
    val future = Promise[String]().complete(Right(result)).future
    futureWithResult(_(future, result))
  }
  
  "A failed Promise" should {
    val message = "Expected Exception"
    val future = Promise[String]().complete(Left(new RuntimeException(message))).future
    futureWithException[RuntimeException](_(future, message))
  }
  
  "An interrupted Promise" should {
    val message = "Boxed InterruptedException"
    val future = Promise[String]().complete(Left(new InterruptedException(message))).future
    futureWithException[ExecutionException](_(future, message))
  }
  
  "A NonLocalReturnControl failed Promise" should {
    val result = "test value"
    val future = Promise[String]().complete(Left(new NonLocalReturnControl[String]("test", result))).future
    futureWithResult(_(future, result))
  }
  
  def futureWithResult(f: ((Future[Any], Any) => Unit) => Unit) {
    
    "be completed" in { f((future, _) => future.isCompleted mustBe (true)) }
    
    "contain a value" in { f((future, result) => future.value mustBe (Some(Right(result)))) }
    
    "return result with 'blocking'" in { f((future, result) => blocking(future, defaultTimeout) mustBe (result)) }
    
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
        future.value.get.left.get.getMessage mustBe (message)
      })
    }
    
    "throw exception with 'blocking'" in {
      f {
        (future, message) =>
        intercept[E] {
          blocking(future, defaultTimeout)
        }.getMessage mustBe (message)
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







