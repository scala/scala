//> using options -Xasync

object Test extends scala.tools.partest.JUnitTest(classOf[scala.async.run.ifelse4.IfElse4Spec])

package scala.async.run.ifelse4 {

  import language.{reflectiveCalls, postfixOps, existentials}
  import scala.concurrent.{Future, ExecutionContext, Await}
  import scala.concurrent.duration._
  import scala.tools.testkit.async.Async.{async, await}
  import org.junit.Test
  import org.junit.Assert._


  class TestIfElse4Class {

    import ExecutionContext.Implicits.global

    class F[A]
    class S[A](val id: String)
    trait P

    case class K(f: F[_])

    def result[A](f: F[A]) = async {
      new S[A with P]("foo")
    }

    def run(k: K) = async {
      val res = await(result(k.f))
      // these triggered a crash with mismatched existential skolems
      //  found   : S#10272[_$1#10308 with String#137] where type _$1#10308
      //  required: S#10272[_$1#10311 with String#137] forSome { type _$1#10311 }

      // This variation of the crash could be avoided by fixing the over-eager
      // generation of states in `If` nodes, which was caused by a bug in label
      // detection code.
      if(true) {
        identity(res)
      }

      // This variation remained after the aforementioned fix, however.
      // It was fixed by manually typing the `Assign(liftedField, rhs)` AST,
      // which is how we avoid these problems through the rest of the ANF transform.
      if(true) {
        identity(res)
        await(result(k.f))
      }
      res
    }
  }

  class IfElse4Spec {

    @Test
    def `await result with complex type containing skolem`(): Unit = {
      val o = new TestIfElse4Class
      val fut = o.run(o.K(null))
      val res = Await.result(fut, 2 seconds)
      assertEquals("foo", res.id)
    }
  }

}
