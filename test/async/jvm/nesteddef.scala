//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.nesteddef.NestedDef])

package scala.async.run.nesteddef {

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
  class NestedDef {

    @Test
    def nestedDef(): Unit = {
      val result = block(async {
        val a = 0
        val x = await(a) - 1
        val local = 43
        def bar(d: Double) = -d + a + local
        def foo(z: Any) = (a.toDouble, bar(x).toDouble, z)
        foo(await(2))
      })
      assertEquals((0d, 44d, 2), result)
    }


    @Test
    def nestedFunction(): Unit = {
      val result = block(async {
        val a = 0
        val x = await(a) - 1
        val local = 43
        val bar = (d: Double) => -d + a + local
        val foo = (z: Any) => (a.toDouble, bar(x).toDouble, z)
        foo(await(2))
      })
      assertEquals((0d, 44d, 2), result)
    }

    // We must lift `foo` and `bar` in the next two tests.
    @Test
    def nestedDefTransitive1(): Unit = {
      val result = block(async {
        val a = 0
        val x = await(a) - 1
        def bar = a
        def foo = bar
        foo
      })
      assertEquals(0, result)
    }

    @Test
    def nestedDefTransitive2(): Unit = {
      val result = block(async {
        val a = 0
        val x = await(a) - 1
        def bar = a
        def foo = bar
        0
      })
      assertEquals(0, result)
    }


    // checking that our use/definition analysis doesn't cycle.
    @Test
    def mutuallyRecursive1(): Unit = {
      val result = block(async {
        val a = 0
        val x = await(a) - 1
        def foo: Int = if (true) 0 else bar
        def bar: Int = if (true) 0 else foo
        bar
      })
      assertEquals(0, result)
    }

    // checking that our use/definition analysis doesn't cycle.
    @Test
    def mutuallyRecursive2(): Unit = {
      val result = block(async {
        val a = 0
        def foo: Int = if (true) 0 else bar
        def bar: Int = if (true) 0 else foo
        val x = await(a) - 1
        bar
      })
      assertEquals(0, result)
    }
  }

}
