//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.live.LiveVariablesSpec])

package scala.async.run.live {
  import org.junit.Test
  import org.junit.Assert._

  import scala.concurrent._
  import duration.Duration
  import scala.tools.testkit.async.Async.{async, await}
  import scala.collection.immutable
  object TestUtil {
    import language.implicitConversions
    implicit def lift[T](t: T): Future[T] = Future.successful(t)
    def block[T](f: Future[T]): T = Await.result(f, Duration.Inf)
  }
  import TestUtil._

  case class Cell[T](v: T)

  class Meter(val len: Long) extends AnyVal

  case class MCell[T](var v: T)

  class LiveVariablesSpec {
    implicit object testingEc extends ExecutionContext {
      var lastStateMachine: Any = _
      var lastFailure: Throwable = _
      def live[T: reflect.ClassTag]: List[T] = {
        val instance = lastStateMachine
        val flds = instance.getClass.getDeclaredFields
        val filterClass = reflect.classTag[T].runtimeClass
        flds.toList.flatMap { fld =>
          fld.setAccessible(true)
          val value = fld.get(instance)
          if (filterClass.isInstance(value)) {
            value.asInstanceOf[T] :: Nil
          } else Nil
        }
      }
      override def execute(runnable: Runnable): Unit = try {
        lastStateMachine = reflectivelyExtractStateMachine(runnable)
        runnable.run()
      } catch {
        case t: Throwable => throw new RuntimeException(t)
      }
      override def reportFailure(cause: Throwable): Unit = {
        lastFailure = cause
      }

      private def reflectivelyExtractStateMachine(runnable: Runnable) = {
        assert(runnable.getClass == Class.forName("scala.concurrent.impl.Promise$Transformation"), runnable.getClass)
        val fld = runnable.getClass.getDeclaredField("_fun")
        fld.setAccessible(true)
        val stateMachine = fld.get(runnable)
        assert(stateMachine.getClass.getName.contains("stateMachine"), stateMachine.getClass)
        stateMachine
      }
    }

    @Test
    def `zero out fields of reference type`(): Unit = {
      def live: Set[Any] = {
        testingEc.live[Cell[_]].map(_.v).toSet
      }

      def m3() = async {
        val _0: Any = await(Cell(0))
        val _1: Cell[Int] = await(Cell(1))
        identity(_1)
        val _2 = await(Cell(2))

        identity(_1)
        assertEquals(Set(0, 1), live)
        val res = await(_2.toString)

        assertEquals(Set(0), live)
        identity(_0)
        res
      }

      assert(block(m3()) == "Cell(2)")
    }

    @Test
    def `zero out fields after use in loop`(): Unit = {
      val f = Cell(1)

      def live: Set[Any] = {
        testingEc.live[Cell[_]].map(_.v).toSet
      }

      def m3() = async {
        val _0: Any = await(Cell(0))
        val _1: Cell[Int] = await(Cell(1))
        var i = 0
        while (i < 5) {
          identity(await(_1))
          assertEquals("in loop", Set(0, 1), live)
          i += 1
        }
        assertEquals("after loop", Set(0), live)
        identity(_0)
        await(())
        assertEquals("end of block", Set(), live)
        ()
      }

      block(m3())
    }

      @Test
      def `don't zero captured fields captured lambda`(): Unit = {
        def live: Set[Any] = {
          testingEc.live[Cell[_]].map(_.v).toSet
        }

        def m3() = async {
          val _1 = Cell(1)
          val _2 = Cell(2)
          val _3 = Cell(3)
          val _4 = Cell(4)
          val _5 = Cell(5)
          val _6 = Cell(6)
          await(0)
          _1.toString.reverse
          val fun = () => assert(_2 != null)
          class LocalClass { assert(_3 != null) }
          object localObject { assert(_4 != null) }
          def localDef = { assert(_5 != null) }
          lazy val localLazy = { assert(_6 != null) }
          await(0)
          assertEquals("after capture", Set(2, 3, 4, 5, 6), live)
          fun()
          new LocalClass()
          localObject
        }

        block(m3())
      }


    @Test
    def `capture bug`(): Unit = {
      sealed trait Base
      case class B1() extends Base
      case class B2() extends Base
      val outer = List[(Base, Int)]((B1(), 8))

      def getMore(b: Base) = 4

      def baz = async {
        outer.head match {
          case (a @ B1(), r) => {
            val ents = await(getMore(a))

            { () =>
              // println(a)
              assert(a ne null)
            }
          }
          case (b @ B2(), x) =>
            () => ???
        }
      }
      block(baz).apply()
    }

    // https://github.com/scala/async/issues/104
    @Test def dontNullOutVarsOfTypeNothing_t104(): Unit = {
      def errorGenerator(randomNum: Double) = {
        Future {
          if (randomNum < 0) {
            throw new IllegalStateException("Random number was too low!")
          } else {
            throw new IllegalStateException("Random number was too high!")
          }
        }
      }
      def randomTimesTwo = async {
        val num = _root_.scala.math.random()
        if (num < 0 || num > 1) {
          await(errorGenerator(num))
        }
        num * 2
      }
      block(randomTimesTwo) // was: NotImplementedError
    }
  }
}
