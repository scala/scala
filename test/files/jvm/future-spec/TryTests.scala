// This is a port of the com.twitter.util Try spec.
// --
// It lives in the future-spec directory simply because it requires a specs-like
// DSL which has already been minimally implemented for the future spec tests.

import scala.util.{Try,Success,Failure}

class TryTests extends MinimalScalaTest {
  class MyException extends Exception
  val e = new Exception("this is an exception")

  "Try()" should {
    "catch exceptions and lift into the Try type" in {
      Try[Int](1) mustEqual Success(1)
      Try[Int] { throw e } mustEqual Failure(e)
    }
  }

  "Try" should {
    "recoverWith" in {
      val myException = new MyException
      Success(1) recoverWith { case _ => Success(2) } mustEqual Success(1)
      Failure(e) recoverWith { case _ => Success(2) } mustEqual Success(2)
      Failure(e) recoverWith { case _ => Failure(e) } mustEqual Failure(e)
    }

    "getOrElse" in {
      Success(1) getOrElse 2 mustEqual 1
      Failure(e) getOrElse 2 mustEqual 2
    }

    "orElse" in {
      Success(1) orElse Success(2) mustEqual Success(1)
      Failure(e) orElse Success(2) mustEqual Success(2)
    }

    "map" in {
      "when there is no exception" in {
        Success(1) map(1+) mustEqual Success(2)
        Failure[Int](e) map(1+) mustEqual Failure(e)
      }

      "when there is an exception" in {
        Success(1) map(_ => throw e) mustEqual Failure(e)

        val e2 = new Exception
        Failure[Int](e) map(_ => throw e2) mustEqual Failure(e)
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
        Failure[Int](e) flatMap(x => Success(1 + x)) mustEqual Failure(e)
      }

      "when there is an exception" in {
        Success(1).flatMap[Int](_ => throw e) mustEqual Failure(e)

        val e2 = new Exception
        Failure[Int](e).flatMap[Int](_ => throw e2) mustEqual Failure(e)
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
            i <- Failure[Int](e)
            j <- Success(1)
          } yield (i + j)
          result mustEqual Failure(e)
        }

        "throws after" in {
          val result = for {
            i <- Success(1)
            j <- Failure[Int](e)
          } yield (i + j)
          result mustEqual Failure(e)
        }

        "returns the FIRST Failure" in {
          val e2 = new Exception
          val result = for {
            i <- Failure[Int](e)
            j <- Failure[Int](e2)
          } yield (i + j)
          result mustEqual Failure(e)
        }
      }
    }
  }
}
