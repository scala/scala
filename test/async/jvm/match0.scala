//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.match0.MatchSpec])

package scala.async.run.match0 {

  import language.{reflectiveCalls, postfixOps}

  import org.junit.Test
  import org.junit.Assert._

  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  import scala.tools.testkit.async.Async.{async, await}
  object TestUtil {
    import language.implicitConversions
    implicit def lift[T](t: T): Future[T] = Future.successful(t)
    def block[T](f: Future[T]): T = Await.result(f, Duration.Inf)
  }
  import TestUtil._

  class TestMatchClass {

    def m1(x: Int): Future[Int] = Future {
      x + 2
    }

    def m2(y: Int): Future[Int] = async {
      val f = m1(y)
      var z = 0
      y match {
        case 10 =>
          val x1 = await(f)
          z = x1 + 2
        case 20 =>
          val x2 = await(f)
          z = x2 - 2
      }
      z
    }

    def m3(y: Int): Future[Int] = async {
      val f = m1(y)
      var z = 0
      y match {
        case 0 =>
          val x2 = await(f)
          z = x2 - 2
        case 1 =>
          val x1 = await(f)
          z = x1 + 2
      }
      z
    }
  }


  class MatchSpec {

    @Test def `support await in a simple match expression`(): Unit = {
      val o = new TestMatchClass
      val fut = o.m2(10) // matches first case
      val res = Await.result(fut, 2 seconds)
      assertEquals(14, res)
    }

    @Test def `support await in a simple match expression 2`(): Unit = {
      val o = new TestMatchClass
      val fut = o.m3(1) // matches second case
      val res = Await.result(fut, 2 seconds)
      assertEquals(5, res)
    }

    @Test def `support await in a match expression with binds`(): Unit = {
      val result = block(async {
        val x = 1
        Option(x) match {
          case op @ Some(x) =>
            assert(op.contains(1))
            x + await(x)
          case None => await(0)
        }
      })
      assertEquals(2, result)
    }

    @Test def `support await referring to pattern matching vals`(): Unit = {
      val result = block(async {
        val x = 1
        val opt = Some("")
        await(0)
        val o @ Some(y) = opt

        {
          val o @ Some(y) = Some(".")
        }

        await(0)
        await((o, y.isEmpty))
      })
      assertEquals((Some(""), true), result)
    }

    @Test def `await in scrutinee`(): Unit = {
      val result = block(async {
        await(if ("".isEmpty) await(1) else ???) match {
          case x if x < 0 => ???
          case y: Int => y * await(3)
        }
      })
      assertEquals(3, result)
    }

    @Test def duplicateBindName(): Unit = {
      def m4(m: Any) = block(async {
        m match {
          case buf: String =>
            await(0)
          case buf: Double =>
            await(2)
          case x => throw new MatchError(x)
        }
      })

      assertEquals(0, m4(""))
    }

    @Test def bugCastBoxedUnitToStringMatch(): Unit = {
      def foo = block(async {
        val p2 = await(5)
        "foo" match {
          case p3: String =>
            p2.toString
        }
      })
      assertEquals("5", foo)
    }

    @Test def bugCastBoxedUnitToStringIf(): Unit = {
      def foo = block(async {
        val p2 = await(5)
        if (true) p2.toString else p2.toString
      })
      assertEquals("5", foo)
    }
  }

}
