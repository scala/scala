package scala.tools.nsc
package async

import java.io.File
import java.lang.reflect.InvocationTargetException
import java.nio.file.{Files, Paths}
import java.util.concurrent.CompletableFuture

import org.junit.Assert.assertEquals
import org.junit.{Assert, Ignore, Test}

import scala.annotation.StaticAnnotation
import scala.concurrent.duration.Duration
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.partest.async.AsyncStateMachine

class AnnotationDrivenAsync {
  @Test
  def testBasicScalaConcurrent(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[Int] = async { await(f(1)) + await(f(2)) }
        |  def test1: Future[Int] = async { await(f(1)) + await(f(2)) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(3, run(code))
  }

  @Test
  def patternTailPosition(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def test = async {
        |     {
        |       await(f(1))
        |       "foo" match {
        |         case x if "".isEmpty => x
        |       }
        |     }: AnyRef
        |  }
        |}
        |""".stripMargin
    assertEquals("foo", run(code))
  }

  @Test
  @Ignore // TODO XASYNC
  def testBoxedUnitNotImplemented(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.successful
        |class A {
        |  def f = successful(this)
        |}
        |object Test {
        |  val data = List(("0", "0"))
        |  def test = async {
        |    val s1 = await(new A().f)
        |    s1.toString
        |    val s2 = await(s1.f)
        |    s2.toString
        |    val it = data.iterator
        |    while (it.hasNext) {
        |      val v = it.next()
        |      v match {
        |        case (x, y) =>
        |          "".isEmpty
        |          val r1 = await(s1.f).toString
        |          val r2 = await(s1.f).toString
        |          (r1, r2)
        |          val it = Nil.iterator
        |          while (it.hasNext) {
        |            val v = it.next()
        |            val r = await(s1.f).equals(v)
        |          }
        |      }
        |    }
        |  }
        |}
        |""".stripMargin
     assertEquals((), run(code))
  }

  @Test
  def testMixedBagNPE(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.successful
        |class A {
        |  def f = successful(this)
        |}
        |object Test {
        |  val data = List(("0", "0"))
        |  def test = async {
        |    val s1 = await(new A().f)
        |    s1.toString
        |    val s2 = await(s1.f)
        |    s2.toString
        |    val it = data.iterator
        |    while (it.hasNext) {
        |      val v = it.next()
        |      v match {
        |        case (x, y) =>
        |          "".isEmpty
        |          val r1 = await(s2.f).toString
        |          val r2 = await(s2.f).toString
        |          (r1, r2)
        |          val it = List("").iterator
        |          while (it.hasNext) {
        |            val v = it.next()
        |            val r = await(s2.f).equals(v)
        |          }
        |      }
        |    }
        |  }
        |}
        |""".stripMargin
     assertEquals((), run(code))
  }

  @Test
  def patternTailPositionMatchEndCast(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |trait A1
        |trait B1
        |trait B2
        |trait Z1
        |class C1 extends B1 with A1 with Z1
        |class C2 extends A1 with B2 with Z1
        |class C2A extends C2
        |class C2B extends C2
        |object Test {
        |  def test = async[Z1] {
        |    val result = if ("foo".isEmpty) {
        |      await(f(1))
        |      null : Z1
        |    } else {
        |      (0: Any) match {
        |        case 0 =>
        |          await(f(1))
        |          null: C2 with Z1
        |        case _ =>
        |          null: C1 with Z1
        |      }
        |    }
        |    result
        |  }
        |}
        |""".stripMargin
    assertEquals(null, run(code))
  }

  @Test
  def awaitTyped(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def test = async {(("msg: " + await(f(0))): String).toString}
        |}
        |""".stripMargin
    assertEquals("msg: 0", run(code))
  }

  @Test
  def avoidLiftingTryIntoExpression(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def test = async {
        |    //var info = ""
        |    var info = try {
        |      "body1"
        |    } catch {
        |      case _: Throwable => "fallback"
        |    }
        |    var info1 = ""
        |    info1 = try {
        |      "body2"
        |    } catch {
        |      case _: Throwable => "fallback"
        |    }
        |
        |    await(f(0))
        |    (info, info1)
        |  }
        |}
        |""".stripMargin
    assertEquals(("body1", "body2"), run(code))
  }

  @Test
  def avoidLiftingTryIntoExpression2(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def test = async {
        |    await(f(1))
        |    try {
        |      "body"
        |    } catch {
        |      case _: Throwable =>
        |        "catch"
        |    }
        |  }
        |}
        |""".stripMargin
    assertEquals("body", run(code))
  }

  @Test
  def avoidWhileExprPosition(): Unit = {
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def test = async {
        |    if ("".isEmpty) {
        |      ()
        |    } else {
        |      var continue = true
        |      while (continue) {
        |        continue = await(f(false))
        |      }
        |    }
        |    await(f("result"))
        |  }
        |}
        |""".stripMargin
    assertEquals("result", run(code))
  }

  @Test
  def whileExpr1(): Unit = {
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  var continue = true
        |  def test = async { (await(test1), await(test2)) }
        |  def test1 = async {
        |    while(continue) {
        |      continue = false
        |      await(f(()))
        |    }
        |    await(f("result1"))
        |  }
        |  def test2 = async {
        |    await(f(""))
        |    while(continue) {
        |      continue = false
        |      await(f(()))
        |    }
        |    await(f("result2"))
        |  }
        |}
        |""".stripMargin
    assertEquals(("result1", "result2"), run(code))
  }

  @Test
  def genericUnitTypedAssert(): Unit = {
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def finish[T](t: T): T = t
        |  def test = async {
        |    finish(this match { case _ if "".isEmpty => (); case _ => await(f(())) })
        |  }
        |}
        |""".stripMargin
    assertEquals((), run(code))
  }

  @Test
  def unitTypedValAwaitingWhileRhs(): Unit = {
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def finish[T](t: T): T = t
        |  var continue = true
        |  def test = async {
        |    val x = while(continue) {
        |      await(f(()))
        |      continue = false
        |      ()
        |    }
        |    "result"
        |  }
        |}
        |""".stripMargin
    assertEquals("result", run(code))
  }

  @Test
  def nestedBlock(): Unit = {
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def finish[T](t: T): T = t
        |  var continue = true
        |  def condition = true
        |  def test = async {
        |    if (condition) {
        |      toString
        |      if (condition) {
        |        scala.runtime.BoxedUnit.UNIT
        |      } else {
        |        ( { if (condition) await(f("")) ; scala.runtime.BoxedUnit.UNIT } : scala.runtime.BoxedUnit )
        |      }
        |    } else {
        |      identity(())
        |    }
        |  }
        |}
        |""".stripMargin
    assertEquals((), run(code))
  }

  @Test
  def matchWithIf(): Unit = {
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def finish[T](t: T): T = t
        |  def condition = true
        |  def scrut: Some[AnyRef] = Some("")
        |  def test = async {
        |    scrut match {
        |      case Some(_) =>
        |        val a = "a"
        |        val x = if (condition) "then" else { await(f("")); "else" }
        |        identity((a, x))
        |      }
        |  }
        |}
        |""".stripMargin
    assertEquals(("a", "then"), run(code))
  }

  @Test
  def testBooleanAndOr(): Unit = {
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  var counter = 0
        |  def ordered(i: Int, b: Boolean): Boolean = { assert(counter == i, (counter, i)); counter += 1; b }
        |  def test: Future[Any] = async {
        |    counter = 0; assert(!(ordered(0, false) && await(f(ordered(-1,  true)))))
        |    counter = 0; assert(!(ordered(0, false) && await(f(ordered(-1, false)))))
        |    counter = 0; assert( (ordered(0,  true) && await(f(ordered( 1,  true)))))
        |    counter = 0; assert(!(ordered(0,  true) && await(f(ordered( 1, false)))))
        |    counter = 0; assert( (ordered(0, false) || await(f(ordered( 1,  true)))))
        |    counter = 0; assert(!(ordered(0, false) || await(f(ordered( 1, false)))))
        |    counter = 0; assert( (ordered(0,  true) || await(f(ordered(-1, false)))))
        |    counter = 0; assert( (ordered(0,  true) || await(f(ordered(-1,  true)))))
        |    ()
        |  }
        |}
        |""".stripMargin
    assertEquals((), run(code))
  }

  @Test
  def testBasicScalaConcurrentViaMacroFrontEnd(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[Int] = async { await(f(1)) + await(f(2)) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(3, run(code))
  }

  @Test
  def testSyncOptimizationScalaConcurrentViaMacroFrontEnd(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def stackDepth = Thread.currentThread().getStackTrace.length
        |
        |  def test: Future[Unit] = async {
        |    val thread1 = Thread.currentThread
        |    val stackDepth1 = stackDepth
        |
        |    val f = await(Future.successful(1))
        |    val thread2 = Thread.currentThread
        |    val stackDepth2 = stackDepth
        |    assert(thread1 == thread2)
        |    assert(stackDepth1 == stackDepth2)
        |  }
        |
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals((), run(code))
  }

  @Test
  def testBasicScalaConcurrentValueClass(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |class IntWrapper(val value: String) extends AnyVal
        |object Test {
        |  def test: Future[String] = async { await(inner).value }
        |  def inner: Future[IntWrapper] = async { await(f(new IntWrapper("hola"))) }
        |}
        |""".stripMargin
    assertEquals("hola", run(code))
  }

  @Test
  def testMatchBig(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |
        |object Test {
        |  def test: Future[Int] = async {
        |    val x: Option[Either[Object, (String, String)]] = Some(Right(("a", "b")))
        |    x match {
        |      case Some(Left(_)) => 1
        |      case Some(Right(("a", "c"))) => 2
        |      case Some(Right(("a", "e"))) => 3
        |      case Some(Right(("a", x))) if "ab".isEmpty => 4
        |      case Some(Right(("a", "b"))) => await(f(5))
        |      case Some(Right((y, x))) if x == y => 6
        |      case Some(Right((_, _))) => await(f(7))
        |      case None => 8
        |    }
        | }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(5, run(code))
  }
 @Test
  def testMatchSmall(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |
        |object Test {
        |  def test: Future[Int] = async {
        |    val x: Option[Either[Object, (String, String)]] = Some(Right(("a", "b")))
        |    (x: @unchecked) match {
        |      case Some(Right(("a", "b"))) => await(f(5))
        |      case None => 8
        |    }
        | }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(5, run(code))
  }


  @Test
  def testBasicScalaConcurrentCapture(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[(String, Int, Int)] = async { var x = "init"; val y = await(f(1)); class C { x = x + "_updated" }; new C; (x, y, await(f(2))) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(("init_updated", 1, 2), run(code))
  }

 @Test
  def testLiftedLazyVal(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[(Int, Int, Int)] = async { var i = 0; var z = 0; lazy val foo = { def ii = i; z = -1; ii }; await(f(1)) + await(f(2)); i += 1; (foo, foo, z) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals((1, 1, -1), run(code))
  }

  @Test
  def testWhile1(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def p[T](t: T): T = {println(t); t }
        |  def test: Future[Int] = async {
        |    var sum = 0
        |    var i = 0
        |    while (i < 5) {
        |      var j = 0
        |      while (j < 5) {
        |        sum += await(f(i)) * await(f(j))
        |        j += 1
        |      }
        |      i += 1
        |    }
        |    sum
        |  }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(100, run(code))
  }

  @Test
  def testWhile2(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def p[T](t: T): T = {println(t); t }
        |  def test: Future[Int] = async {
        |    var sum = 0
        |    var i = 0
        |    while (i < 5) {
        |      sum += await(f(i))
        |      i += 1
        |    }
        |    sum
        |  }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(10, run(code))
  }

  @Test
  def testCaseClassLifting(): Unit = {
    // Note: this emits a warning under -Ydebug (which we sometimes manually set below in the compiler setup)
    // https://github.com/scala/scala/pull/8750 will fix this.
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
         import scala.tools.partest.async.Async.{async, await}
         import Future.{successful => f}
         object Test {
           def test = async {
             {
              trait Base { def base = 0}
              await(f(0))
              case class Person(name: String) extends Base
              val fut = f("bob")
              val x = Person(await(fut))
              x.base
              assert(Person.getClass.getName == classOf[Person].getName + "$", (Person.getClass.getName, classOf[Person].getName))
              x.name
            }
           }
         }
        """
    assertEquals("bob", run(code))
  }

  @Test
  def testScalaConcurrentAsyncNested(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def foo[T](a0: Int)(b0: Int*) = s"a0 = $a0, b0 = ${b0.head}"
        |
        |  def test: String = Await.result(async {
        |    var i = 0
        |    def get = async{i += 1; i}
        |    foo[Int](await(get))(await(get) :: await(async(Nil)) : _*)
        |  }, Duration.Inf)
        |}
        |""".stripMargin
    assertEquals("a0 = 1, b0 = 2", run(code))
  }

  @Test
  def testCustomAsync(): Unit = {
    val code = """
       |import scala.tools.nsc.async.{autoawait, customAsync}
       |
       |object Test {
       |  @customAsync
       |  def test: Any = {
       |    val x = reverse("abc")
       |    val y = reverse(x)
       |    (x, y)
       |  }
       |  @autoawait def reverse(a: String) = a.reverse
       |}
       |
       |""".stripMargin
    assertEquals(("cba", "abc"), run(code))
  }

  @Test
  def testMixedAsync(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global, scala.tools.partest.async.Async._
      |
      |object Test {
      |  @customAsync
      |  def test: Any = {
      |    class C {
      |      def repeat(s: String, i: Int): Future[String] = async {
      |        if (i == 0) s
      |        else await(repeat(s, i - 1)) + s
      |      }
      |    }

      |    val x = reverse("abc")
      |    val y = reverse(x)
      |    val z = Await.result(new C().repeat("-", 5), Duration.Inf)
      |    (x, y, z)
      |  }
      |  @autoawait def reverse(a: String) = a.reverse
      |}
      |
      |""".stripMargin
    assertEquals(("cba", "abc", "------"), run(code))
  }


  @Test
  def testExtractor(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test {
      |
      |  object Extractor1 {
      |    @autoawait def unapply(a: String) = Some((a + 1, a + 2))
      |  }
      |  object Extractor2 {
      |    @autoawait def unapply(a: String) = Some(a + 3)
      |  }
      |  @customAsync
      |  def test: Any = {
      |    @autoawait def id(a: String) = a
      |
      |    println("Test.test")
      |    val r1 = Predef.identity("blerg") match {
      |      case x if " ".isEmpty                                   => "case 2: " + x
      |      case Extractor1(Extractor2(x), y: String) if x == "xxx" => "case 1: " + x + ":" + y
      |        x match {
      |          case Extractor1(Extractor2(x), y: String) =>
      |          case _                                    =>
      |        }
      |      case Extractor2(x)                                      => "case 3: " + x
      |    }
      |    r1
      |  }
      |}
      |
      |""".stripMargin
    assertEquals("case 3: blerg3", run(code))
  }


  @Test
  def testLocalModule(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test {
      |  @customAsync def test: Any = {
      |    object Foo {
      |      @autoawait def id(a: String) = a
      |    }
      |    (Foo.id("a"), Foo.id("b"))
      |  }
      |}
      |
      |""".stripMargin
    assertEquals(("a", "b"), run(code))
  }

  @Test
  def testGuard(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test extends App {
      |  @customAsync
      |  def test: Any = {
      |    @autoawait def id[A](a: A) = a
      |
      |    "" match {
      |      case _ if id(false) => ???;
      |      case _              => id("okay")
      |    }
      |  }
      |}
      |
      |""".stripMargin
    assertEquals("okay", run(code))
  }


  @Test
  def testNestedMatchExtractor(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test extends App {
      |  @customAsync def test = {
      |    object Extractor {
      |      @autoawait def unapply(a: String) = Some((a, a))
      |    }
      |    "" match {
      |      case _ if "".isEmpty =>
      |        "" match {
      |          case Extractor(a, b) => a == b
      |        }
      |    }
      |  }
      |}
      |
      |""".stripMargin
    assertEquals(true, run(code))
  }

  @Test
  def completableFuture(): Unit = {
    val code = """
      |import java.util.concurrent._
      |import scala.tools.nsc.async.CompletableFutureAwait._
      |
      |object Test {
      |  val pool = java.util.concurrent.Executors.newWorkStealingPool()
      |  def f1 = CompletableFuture.supplyAsync(() => 1, pool)
      |  def test = {
      |    async(pool) {
      |      var i = 0
      |      while (i < 100) {
      |        i += await(f1)
      |      }
      |      i
      |    }
      |  }
      |}
      |
      |""".stripMargin
    assertEquals(100, run(code))
  }

  @Test
  def testNothingTypedExpr(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def test: Future[Throwable] = async { if ("".isEmpty) {await(f("")); throw new RuntimeException("boo!")} else ??? }.failed
        |}
        |""".stripMargin
    run(code) match {
      case re: RuntimeException => assert(re.getMessage == "boo!")
      case _ => Assert.fail()
    }
  }

  @Test def testByNameOwner(): Unit = {
    val result = run(
      """
      import scala.tools.nsc.async.{autoawait, customAsync}

      object Bleh {
        @autoawait def asyncCall(): Int = 0
        def byName[T](fn: => T): T = fn
      }
      object Boffo {
        @autoawait @customAsync def jerk(): Unit = {
          val pointlessSymbolOwner = 1 match {
            case _ =>
              Bleh.asyncCall()
              Bleh.byName {
                val whyDoHateMe = 1
                whyDoHateMe
              }
          }
        }
      }
      object Test {
        @customAsync def test() = Boffo.jerk()
      }
      """)
  }

  @Test def testByNameOwner2(): Unit = {
    val result = run(
      """
      import scala.tools.nsc.async.{autoawait, customAsync}
      object Bleh {
        @autoawait def bleh = Bleh
        def byName[T](fn: => T): T = fn
      }
      object Boffo {
        @autoawait @customAsync def slob(): Unit = {
          val pointlessSymbolOwner =  {
              Bleh.bleh.byName {
                val whyDoHateMeToo = 1
                whyDoHateMeToo
              }
          }
        }
      }
      object Test {
        @customAsync def test() = Boffo.slob()
      }
      """)
  }

  @Test def testRewrittenApply(): Unit = {
    val result = run(
      """
        |import scala.tools.nsc.async.{autoawait, customAsync}
        |object O {
        |  case class Foo(a: Any)
        |}
        |object Test {
        |  @autoawait def id(a: String) = a
        |  @customAsync
        |  def test = {
        |    O.Foo
        |    id("foo") + id("bar")
        |    O.Foo(1)
        |  }
        |}
        | """.stripMargin)
    assertEquals("Foo(1)", result.toString)
  }

  @Test def testIsInstanceOfType(): Unit = {
    val result = run(
      """ import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val o = new Outer
        |     id("foo") + id("bar")
        |     ("": Object).isInstanceOf[o.type]
        |   }
        | }
        | """.stripMargin)
    assertEquals(false, result)
  }

  @Test def testIsInstanceOfTerm(): Unit = {
    val result = run(
      """import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val o = new Outer
        |     id("foo") + id("bar")
        |     o.isInstanceOf[Outer]
        |   }
        | }
        | """.stripMargin)
    assertEquals(true, result)
  }

  @Test def testArrayLocalModule(): Unit = {
    val result = run(
      """ import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val O = ""
        |     id("foo") + id("bar")
        |     new Array[O.type](0)
        |   }
        | }
        | """.stripMargin)
    assertEquals(classOf[Array[String]], result.getClass)
  }

  @Test def testLambdaLiftClash(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |object Test {
        |  def test: Future[Int] = async {
        |    def foo = 42
        |    await(f("")); // so that the preceding def will be lifted to foo$N
        |
        |    {
        |      // lambdalift will later lift this to foo$N.
        |      def foo = 43
        |      foo
        |    };
        |    foo
        |  }
        |}
        |""".stripMargin
    // If async and lambdalift phase both use the compilation units FreshNameCreator, we get foo$1 and foo$2, no clash!
    assertEquals(42, run(code))
  }

  @Test def testOutputMonad(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.nsc.async._
        |import OutputAwait._
        |
        |object Test {
        |  def v1 = Output("v1", ("line" -> "1"))
        |  def v2 = Output("v2", ("line" -> "2"), ("foo", "bar"))
        |  def test: Output[String] = writing {
        |    value(v1) + value(v2)
        |  }
        |}
        |""".stripMargin
    // If async and lambdalift phase both use the compilation units FreshNameCreator, we get foo$1 and foo$2, no clash!
    assertEquals("Output(Some(v1v2),Map(line -> Vector(1, 2), foo -> Vector(bar)))", run(code).toString)
  }

  // Handy to debug the compiler or to collect code coverage statistics in IntelliJ.
  @Test
  @Ignore
  def testManualRunPartestUnderJUnit(): Unit = {
    import scala.collection.JavaConverters._
    for (path <- List(Paths.get("../async/run"), Paths.get("../async/neg"))) {
      for (file <- Files.list(path).iterator.asScala) {
        if (file.getFileName.toString.endsWith(".scala")) {
          val code = new String(Files.readAllBytes(file))
          run(code, compileOnly = true)
        }
      }
    }
  }

  private def createTempDir(): File = {
    val f = File.createTempFile("output", "")
    f.delete()
    f.mkdirs()
    f
  }

  def run(code: String, compileOnly: Boolean = false): Any = {
    val out = createTempDir()
    try {
      val reporter = new StoreReporter {
        override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
          if (severity == INFO) println(msg)
          else super.info0(pos, msg, severity, force)
        }
      }
      val settings = new Settings(println(_))
      settings.async.value = true
      settings.outdir.value = out.getAbsolutePath
      settings.embeddedDefaults(getClass.getClassLoader)

      // settings.debug.value = true
      // settings.uniqid.value = true
      // settings.processArgumentString("-Xprint:typer,posterasure,async -nowarn")
      // settings.log.value = List("async")

      // NOTE: edit ANFTransform.traceAsync to `= true` to get additional diagnostic tracing.

      val isInSBT = !settings.classpath.isSetByUser
      if (isInSBT) settings.usejavacp.value = true
      val global = new Global(settings, reporter) {
        self =>

        object late extends {
          val global: self.type = self
        } with AnnotationDrivenAsyncPlugin

        override protected def loadPlugins(): List[Plugin] = late :: Nil
      }
      import global._

      val run = new Run
      val source = newSourceFile(code)
      run.compileSources(source :: Nil)
      if (compileOnly) return null
      def showInfo(info: StoreReporter#Info): String = {
        Position.formatMessage(info.pos, info.severity.toString.toLowerCase + " : " + info.msg, false)
      }
      Assert.assertTrue(reporter.infos.map(showInfo).mkString("\n"), !reporter.hasErrors)
      Assert.assertTrue(reporter.infos.map(showInfo).mkString("\n"), !reporter.hasWarnings)
      val loader = new URLClassLoader(Seq(new File(settings.outdir.value).toURI.toURL), global.getClass.getClassLoader)
      val cls = loader.loadClass("Test")
      val result = try {
        cls.getMethod("test").invoke(null)
      } catch {
        case ite: InvocationTargetException => throw ite.getCause
        case _: NoSuchMethodException =>
          cls.getMethod("main", classOf[Array[String]]).invoke(null, null)
      }
      result match {
        case t: scala.concurrent.Future[_] =>
          scala.concurrent.Await.result(t, Duration.Inf)
        case cf: CustomFuture[_] =>
          cf._block
        case cf: CompletableFuture[_] =>
          cf.get()
        case value => value
      }
    } catch {
      case ve: VerifyError =>
        val asm = out.listFiles().filter(_.getName.contains("stateMachine")).flatMap { file =>
          import scala.sys.process._
          val javap = List("/usr/local/bin/javap", "-v", file.getAbsolutePath).!!
          val asmp = AsmUtils.textify(AsmUtils.readClass(file.getAbsolutePath))
          javap :: asmp :: Nil
        }.mkString("\n\n")
        throw new AssertionError(asm, ve)
    } finally {
      scala.reflect.io.Path.apply(out).deleteRecursively()
    }
  }
}

abstract class AnnotationDrivenAsyncPlugin extends Plugin {

  import global._

  override val components: List[PluginComponent] = List(new PluginComponent with TypingTransformers {
    val global: AnnotationDrivenAsyncPlugin.this.global.type = AnnotationDrivenAsyncPlugin.this.global

    lazy val asyncModuleSym = symbolOf[CustomFuture.type]
    lazy val awaitSym = symbolOf[CustomFuture.type].info.member(TermName("_await"))
    lazy val autoAwaitSym = symbolOf[autoawait]
    lazy val customAsyncSym = symbolOf[customAsync]
    lazy val CustomFuture_class = symbolOf[CustomFuture.type]
    lazy val CustomFuture_successful = CustomFuture_class.companionModule.info.member(TermName("_successful"))

    def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = {
        def wrapAwait = {
          localTyper.typedPos(tree.pos) {
            Apply(TypeApply(gen.mkAttributedRef(asyncModuleSym.typeOfThis, awaitSym), TypeTree(tree.tpe) :: Nil), gen.mkMethodCall(CustomFuture_successful, tree :: Nil) :: Nil)
          }
        }
        super.transform(tree) match {
          case ap@Apply(fun, _) if fun.symbol.hasAnnotation(autoAwaitSym) =>
            wrapAwait
          case sel@Select(_, _) if sel.symbol.hasAnnotation(autoAwaitSym) && !(tree.tpe.isInstanceOf[MethodType] || tree.tpe.isInstanceOf[PolyType]) =>
            wrapAwait
          case dd: DefDef if dd.symbol.hasAnnotation(customAsyncSym) =>
            deriveDefDef(dd) {
              rhs =>
                val applyMethod =
                  q"""def apply(tr: _root_.scala.util.Either[_root_.scala.Throwable, _root_.scala.AnyRef]): _root_.scala.Unit = $rhs"""
                val applyMethodMarked = global.async.markForAsyncTransform(dd.symbol, applyMethod, awaitSym, Map.empty)
                val name = TypeName("stateMachine$$async_" + dd.pos.line)
                val wrapped =
                  q"""
                    class $name extends _root_.scala.tools.nsc.async.CustomFutureStateMachine {
                     $applyMethodMarked
                    }
                    new $name().start()
                   """

                val tree =
                  q"""
                      val temp = ${wrapped}
                     temp._block
                    """
                val result = atOwner(dd.symbol) {
                  localTyper.typedPos(dd.pos) {
                    tree
                  }
                }
                result
            }
          case x => x
        }
      }
    }

    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        newTransformer(unit).transformUnit(unit)
      }
    }

    override val runsAfter: List[String] = "refchecks" :: "patmat" :: Nil
    override val phaseName: String = "postpatmat"

  })
  override val description: String = "postpatmat"
  override val name: String = "postpatmat"
}

// Calls to methods with this annotation are translated to `Async.await(Future.successful(<call>))`
// This lets us express and test async boundaries in extractor calls, which one can't do with the async/await macro.
final class autoawait extends StaticAnnotation

final class customAsync extends StaticAnnotation

abstract class CustomFutureStateMachine extends AsyncStateMachine[CustomFuture[AnyRef], scala.util.Either[Throwable, AnyRef]] with Function1[scala.util.Either[Throwable, AnyRef], Unit] {
  private val result$async: CustomPromise[AnyRef] = new CustomPromise[AnyRef](scala.concurrent.Promise.apply[AnyRef]);
  private[this] var _state = 0
  protected def state$async: Int = _state
  protected def state$async_=(i: Int) = _state = i
  def apply(tr$async: R[AnyRef]): Unit

  type F[A] = CustomFuture[A]
  type R[A] = Either[Throwable, A]
  // Adapter methods
  protected def completeFailure(t: Throwable): Unit = result$async._complete(Left(t))
  protected def completeSuccess(value: AnyRef): Unit = result$async._complete(Right(value))
  protected def onComplete(f: F[AnyRef]): Unit = f._onComplete(this)
  protected def getCompleted(f: F[AnyRef]): R[AnyRef] = f._getCompleted
  protected def tryGet(tr: R[AnyRef]): AnyRef = tr match {
    case Right(value) =>
      value
    case Left(throwable) =>
      result$async._complete(tr)
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): CustomFuture[AnyRef] = {
    CustomFuture._unit.asInstanceOf[CustomFuture[AnyRef]]._onComplete(this)
    result$async._future
  }
}
