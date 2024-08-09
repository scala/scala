//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.toughtype.ToughTypeSpec])

package scala.async.run.toughtype {

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

  object ToughTypeObject {

    import ExecutionContext.Implicits.global

    class Inner

    def m2 = async[(List[_], ToughTypeObject.Inner)] {
      val y = await(Future[List[_]](Nil))
      val z = await(Future[Inner](new Inner))
      (y, z)
    }
  }

  class ToughTypeSpec {

    @Test def `propagates tough types`(): Unit = {
      val fut = ToughTypeObject.m2
      val res: (List[_], scala.async.run.toughtype.ToughTypeObject.Inner) = Await.result(fut, 2 seconds)
      assertEquals(Nil, res._1)
    }

    @Test def patternMatchingPartialFunction(): Unit = {
      assertEquals(3, block(async {
        await(1)
        val a = await(1)
        val f = { case x => x + a }: PartialFunction[Int, Int]
        await(f(2))
      }))
    }

    @Test def patternMatchingPartialFunctionNested(): Unit = {
      assertEquals(-3, block(async {
        await(1)
        val neg1 = -1
        val a = await(1)
        val f = { case x => ({case x => neg1 * x}: PartialFunction[Int, Int])(x + a) }: PartialFunction[Int, Int]
        await(f(2))
      }))
    }

    @Test def patternMatchingFunction(): Unit = {
      assertEquals(3, block(async {
        await(1)
        val a = await(1)
        val f = { case x => x + a }: Function[Int, Int]
        await(f(2))
      }))
    }

    @Test def existentialBindIssue19(): Unit = {
      def m7(a: Any) = block(async {
        a match {
          case s: Seq[_] =>
            val x = s.size
            var ss = s
            ss = s
            await(x)
          case x => throw new MatchError(x)
        }
      })
      assertEquals(0, m7(Nil))
    }

    @Test def existentialBind2Issue19(): Unit = {
      import scala.tools.testkit.async.Async._, scala.concurrent.ExecutionContext.Implicits.global
      def conjure[T]: T = null.asInstanceOf[T]

      def m3 = async {
        val p: List[Option[_]] = conjure[List[Option[_]]]
        await(Future(1))
      }

      def m4 = async {
        await(Future[List[_]](Nil))
      }
    }

    @Test def singletonTypeIssue17(): Unit = {
      class A { class B }
      block(async {
        val a = new A
        def foo(b: a.B) = 0
        await(foo(new a.B))
      })
    }

    @Test def existentialMatch(): Unit = {
      trait Container[+A]
      case class ContainerImpl[A](value: A) extends Container[A]
      def foo: Container[_] = block(async {
        val a: Any = List(1)
        a match {
          case buf: Seq[_] =>
            val foo = await(5)
            val e0 = buf(0)
            ContainerImpl(e0)
          case x => throw new MatchError(x)
        }
      })
      foo
    }

    @Test def existentialIfElse0(): Unit = {
      trait Container[+A]
      case class ContainerImpl[A](value: A) extends Container[A]
      def foo: Container[_] = block(async {
        val a: Any = List(1)
        if (true) {
          val buf: Seq[_] = List(1)
          val foo = await(5)
          val e0 = buf(0)
          ContainerImpl(e0)
        } else ???
      })
      foo
    }

    // This test was failing when lifting `def r` with:
    // symbol value m#10864 does not exist in r$1
    //
    // We generated:
    //
    //   private[this] def r$1#5727[A#5728 >: Nothing#157 <: Any#156](m#5731: Foo#2349[A#5728]): Unit#208 = Bippy#2352.this.bar#5532({
    //     m#5730;
    //     ()
    //   });
    //
    // Notice the incorrect reference to `m`.
    //
    // We compensated in `Lifter` by copying `ValDef` parameter symbols directly across.
    //
    // Turns out the behaviour stems from `thisMethodType` in `Namers`, which treats type parameter skolem symbols.
    @Test def nestedMethodWithInconsistencyTreeAndInfoParamSymbols(): Unit = {
      import language.{reflectiveCalls, postfixOps}
      import scala.concurrent.{Future, ExecutionContext, Await}
      import scala.concurrent.duration._
      import scala.tools.testkit.async.Async.{async, await}

      class Foo[A]

      object Bippy {

        import ExecutionContext.Implicits.global

        def bar(f: => Unit): Unit = f

        def quux: Future[String] = ???

        def foo = async {
          def r[A](m: Foo[A])(n: A) = {
            bar {
              locally(m)
              locally(n)
              identity[A] _
            }
          }

          await(quux)

          r(new Foo[String])("")
        }
      }
      Bippy
    }

    @Test
    def ticket63(): Unit = {
      import scala.tools.testkit.async.Async._
      import scala.concurrent.{ ExecutionContext, Future }

      object SomeExecutionContext extends ExecutionContext {
        def reportFailure(t: Throwable): Unit = ???
        def execute(runnable: Runnable): Unit = ???
      }

      trait FunDep[W, S, R] {
        def method(w: W, s: S): Future[R]
      }

      object FunDep {
        implicit def `Something to do with List`[W, S, R](implicit funDep: FunDep[W, S, R]): FunDep[W,List[S],W] =
          new FunDep[W, List[S], W] {
            def method(w: W, l: List[S]) = async {
              val it = l.iterator
              while (it.hasNext) {
                await(funDep.method(w, it.next()))
              }
              w
            }(SomeExecutionContext)
          }
      }

    }

    @Test def ticket66Nothing(): Unit = {
      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global
      val e = new Exception()
      val f: Future[Nothing] = Future.failed(e)
      val f1 = async {
        await(f)
      }
      try {
        Await.result(f1, 5.seconds)
      } catch {
        case `e` =>
      }
    }

    @Test def ticket83ValueClass(): Unit = {
      import scala.tools.testkit.async.Async._
      import scala.concurrent._, duration._, ExecutionContext.Implicits.global
      val f = async {
        val uid = new IntWrapper("foo")
        await(Future(uid))
      }
      val result = Await.result(f, 5.seconds)
      assertEquals(new IntWrapper("foo"), result)
    }

    @Test def ticket86NestedValueClass(): Unit = {
      import ExecutionContext.Implicits.global

      val f = async {
        val a = Future.successful(new IntWrapper("42"))
        await(await(a).plusStr)
      }
      val result = Await.result(f, 5.seconds)
      assertEquals("42!", result)
    }

    @Test def ticket86MatchedValueClass(): Unit = {
      import ExecutionContext.Implicits.global

      def doAThing(param: IntWrapper) = Future(None)

      val fut = async {
        Option(new IntWrapper("value!")) match {
          case Some(valueHolder) =>
            await(doAThing(valueHolder))
          case None =>
            None
        }
      }

      val result = Await.result(fut, 5.seconds)
      assertEquals(None, result)
    }

    @Test def ticket86MatchedParameterizedValueClass(): Unit = {
      import ExecutionContext.Implicits.global

      def doAThing(param: ParamWrapper[String]) = Future(None)

      val fut = async {
        Option(new ParamWrapper("value!")) match {
          case Some(valueHolder) =>
            await(doAThing(valueHolder))
          case None =>
            None
        }
      }

      val result = Await.result(fut, 5.seconds)
      assertEquals(None, result)
    }

    @Test def ticket86PrivateValueClass(): Unit = {
      import ExecutionContext.Implicits.global

      def doAThing(param: PrivateWrapper) = Future(None)

      val fut = async {
        Option(PrivateWrapper.Instance) match {
          case Some(valueHolder) =>
            await(doAThing(valueHolder))
          case None =>
            None
        }
      }

      val result = Await.result(fut, 5.seconds)
      assertEquals(None, result)
    }

    @Test def awaitOfAbstractType(): Unit = {
      import ExecutionContext.Implicits.global

      def combine[A](a1: A, a2: A): A = a1

      def combineAsync[A](a1: Future[A], a2: Future[A]) = async {
        combine(await(a1), await(a2))
      }

      val fut = combineAsync(Future(1), Future(2))

      val result = Await.result(fut, 5.seconds)
      assertEquals(1, result)
    }

    // https://github.com/scala/async/issues/106
    @Test def valueClassT106(): Unit = {
      block(async {
        "whatever value" match {
          case _ =>
            await("whatever return type")
            new IntWrapper("value class matters")
        }
        "whatever return type"
      })
    }
  }

  class IntWrapper(val value: String) extends AnyVal {
    def plusStr = Future.successful(value + "!")
  }
  class ParamWrapper[T](val value: T) extends AnyVal

  class PrivateWrapper private (private val value: String) extends AnyVal
  object PrivateWrapper {
    def Instance = new PrivateWrapper("")
  }


  trait A

  trait B

  trait L[A2, B2 <: A2] {
    def bar(a: Any, b: Any) = 0
  }

}
