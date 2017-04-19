// This is a port of the com.twitter.util Try spec.
// --
// It lives in the future-spec directory simply because it requires a specs-like
// DSL which has already been minimally implemented for the future spec tests.

import scala.util.{Try,Success,Failure}

class TryTests extends MinimalScalaTest {
  class MyException extends Exception("this is an exception")
  class MyError extends Error("this is a fatal error")

  val ex = new MyException
  val er = new MyError

  "Try()" should {
    "catch exceptions and lift into the Try type" in {
      Try[Int](1) mustEqual Success(1)
      Try[Int] { throw ex } mustEqual Failure(ex)
    }

    "not catch errors with the default filter" in {
      intercept[Error] {
        Try[Int] { throw er }
      }
    }

    "catch errors if the filter includes them" in {
      val includeMyError: Try.ExceptionFilter = { case _: MyError => true }

      Try.withFilter(includeMyError) { throw er } mustEqual Failure(er)
    }

    "not catch exceptions if the filter excludes them" in {
      val excludeMyException: Try.ExceptionFilter = { case _: MyException => false }

      intercept[MyException] {
        Try.withFilter[Int](excludeMyException orElse Try.defaultFilter) { throw new MyException }
      }
    }
  }

  "Try" should {
    "recoverWith" in {
      Success(1) recoverWith { case _ => Success(2) } mustEqual Success(1)
      Failure(ex) recoverWith { case _ => Success(2) } mustEqual Success(2)
      Failure(ex) recoverWith { case _ => Failure(ex) } mustEqual Failure(ex)
    }

    "getOrElse" in {
      Success(1) getOrElse 2 mustEqual 1
      Failure(ex) getOrElse 2 mustEqual 2
    }

    "orElse" in {
      Success(1) orElse Success(2) mustEqual Success(1)
      Failure(ex) orElse Success(2) mustEqual Success(2)
    }

    "map" in {
      "when there is no exception" in {
        Success(1) map(1+) mustEqual Success(2)
        Failure[Int](ex) map(1+) mustEqual Failure(ex)
      }

      "when there is an exception" in {
        Success(1) map(_ => throw ex) mustEqual Failure(ex)

        val e2 = new Exception
        Failure[Int](ex) map(_ => throw e2) mustEqual Failure(ex)
      }
      "when there is a fatal exception" in {
        val e3 = new ThreadDeath
        intercept[ThreadDeath] {
          Success(1) map (_ => throw e3)
        }
      }
    }

    "flatMap" in {
      "when there is no exception" in {
        Success(1) flatMap(x => Success(1 + x)) mustEqual Success(2)
        Failure[Int](ex) flatMap(x => Success(1 + x)) mustEqual Failure(ex)
      }

      "when there is an exception" in {
        Success(1).flatMap[Int](_ => throw ex) mustEqual Failure(ex)

        val e2 = new Exception
        Failure[Int](ex).flatMap[Int](_ => throw e2) mustEqual Failure(ex)
      }
      "when there is a fatal exception" in {
        val e3 = new ThreadDeath
        intercept[ThreadDeath] {
          Success(1).flatMap[Int](_ => throw e3)
        }
      }
    }

    "flatten" in {
      "is a Success(Success)" in {
        Success(Success(1)).flatten mustEqual Success(1)
      }

      "is a Success(Failure)" in {
        val e = new Exception
        Success(Failure(e)).flatten mustEqual Failure(e)
      }

      "is a Throw" in {
        val e = new Exception
        Failure[Try[Int]](e).flatten mustEqual Failure(e)
      }
    }

    "for" in {
      "with no Failure values" in {
        val result = for {
          i <- Success(1)
          j <- Success(1)
        } yield (i + j)
        result mustEqual Success(2)
      }

      "with Failure values" in {
        "throws before" in {
          val result = for {
            i <- Failure[Int](ex)
            j <- Success(1)
          } yield (i + j)
          result mustEqual Failure(ex)
        }

        "throws after" in {
          val result = for {
            i <- Success(1)
            j <- Failure[Int](ex)
          } yield (i + j)
          result mustEqual Failure(ex)
        }

        "returns the FIRST Failure" in {
          val e2 = new Exception
          val result = for {
            i <- Failure[Int](ex)
            j <- Failure[Int](e2)
          } yield (i + j)
          result mustEqual Failure(ex)
        }
      }
    }
  }
}
