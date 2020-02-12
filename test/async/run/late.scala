/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.async.run.late

import java.io.File

import junit.framework.Assert.assertEquals
import org.junit.{Assert, Ignore, Test}

import scala.annotation.StaticAnnotation
import scala.annotation.meta.{field, getter}
import scala.async.internal.AsyncId
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.transform.TypingTransformers

// Tests for customized use of the async transform from a compiler plugin, which
// calls it from a new phase that runs after patmat.
class LateExpansion {

  @Test def testRewrittenApply(): Unit = {
    val result = wrapAndRun(
      """
        | object O {
        |   case class Foo(a: Any)
        | }
        | @autoawait def id(a: String) = a
        | O.Foo
        | id("foo") + id("bar")
        | O.Foo(1)
        | """.stripMargin)
    assertEquals("Foo(1)", result.toString)
  }

  @Ignore("Need to use adjustType more pervasively in AsyncTransform, but that exposes bugs in {Type, ... }Symbol's cache invalidation")
  @Test def testIsInstanceOfType(): Unit = {
    val result = wrapAndRun(
      """
        | class Outer
        | @autoawait def id(a: String) = a
        | val o = new Outer
        | id("foo") + id("bar")
        | ("": Object).isInstanceOf[o.type]
        | """.stripMargin)
    assertEquals(false, result)
  }

  @Test def testIsInstanceOfTerm(): Unit = {
    val result = wrapAndRun(
      """
        | class Outer
        | @autoawait def id(a: String) = a
        | val o = new Outer
        | id("foo") + id("bar")
        | o.isInstanceOf[Outer]
        | """.stripMargin)
    assertEquals(true, result)
  }

  @Test def testArrayLocalModule(): Unit = {
    val result = wrapAndRun(
      """
        | class Outer
        | @autoawait def id(a: String) = a
        | val O = ""
        | id("foo") + id("bar")
        | new Array[O.type](0)
        | """.stripMargin)
    assertEquals(classOf[Array[String]], result.getClass)
  }

  @Test def test0(): Unit = {
    val result = wrapAndRun(
      """
        | @autoawait def id(a: String) = a
        | id("foo") + id("bar")
        | """.stripMargin)
    assertEquals("foobar", result)
  }

  @Test def testGuard(): Unit = {
    val result = wrapAndRun(
      """
        | @autoawait def id[A](a: A) = a
        | "" match { case _ if id(false) => ???; case _ => "okay" }
        | """.stripMargin)
    assertEquals("okay", result)
  }

  @Test def testExtractor(): Unit = {
    val result = wrapAndRun(
      """
        | object Extractor { @autoawait def unapply(a: String) = Some((a, a)) }
        | "" match { case Extractor(a, b) if "".isEmpty => a == b }
        | """.stripMargin)
    assertEquals(true, result)
  }

  @Test def testNestedMatchExtractor(): Unit = {
    val result = wrapAndRun(
      """
        | object Extractor { @autoawait def unapply(a: String) = Some((a, a)) }
        | "" match {
        |   case _ if "".isEmpty =>
        |     "" match { case Extractor(a, b) => a == b }
        | }
        | """.stripMargin)
    assertEquals(true, result)
  }

  @Test def testCombo(): Unit = {
    val result = wrapAndRun(
      """
        | object Extractor1 { @autoawait def unapply(a: String) = Some((a + 1, a + 2)) }
        | object Extractor2 { @autoawait def unapply(a: String) = Some(a + 3) }
        | @autoawait def id(a: String) = a
        | println("Test.test")
        | val r1 = Predef.identity("blerg") match {
        |   case x if " ".isEmpty => "case 2: " + x
        |   case Extractor1(Extractor2(x), y: String) if x == "xxx" => "case 1: " + x + ":" + y
        |     x match {
        |       case Extractor1(Extractor2(x), y: String) =>
        |       case _ =>
        |     }
        |   case Extractor2(x) => "case 3: " + x
        | }
        | r1
        | """.stripMargin)
    assertEquals("case 3: blerg3", result)
  }

  @Test def polymorphicMethod(): Unit = {
    val result = run(
      """
        |import scala.async.run.late.{autoawait,lateasync}
        |object Test {
        |  class C { override def toString = "C" }
        |  @autoawait def foo[A <: C](a: A): A = a
        |  @lateasync
        |  def test1[CC <: C](c: CC): (CC, CC) = {
        |    val x: (CC, CC) = 0 match { case _ if false => ???; case _ => (foo(c), foo(c)) }
        |    x
        |  }
        |  def test(): (C, C) = test1(new C)
        |}
        | """.stripMargin)
    assertEquals("(C,C)", result.toString)
  }

  @Test def shadowing(): Unit = {
    val result = run(
      """
        |import scala.async.run.late.{autoawait,lateasync}
        |object Test {
        |  trait Foo
        |  trait Bar extends Foo
        |  @autoawait def boundary = ""
        |  @lateasync
        |  def test: Unit = {
        |    (new Bar {}: Any) match {
        |      case foo: Bar =>
        |        boundary
        |        0 match {
        |          case _ => foo; ()
        |        }
        |        ()
        |    }
        |    ()
        |  }
        |}
        | """.stripMargin)
  }

  @Test def shadowing0(): Unit = {
    val result = run(
      """
        |import scala.async.run.late.{autoawait,lateasync}
        |object Test {
        |  trait Foo
        |  trait Bar
        |  def test: Any = test(new C)
        |  @autoawait def asyncBoundary: String = ""
        |  @lateasync
        |  def test(foo: Foo): Foo = foo match {
        |    case foo: Bar =>
        |      val foo2: Foo with Bar = new Foo with Bar {}
        |      asyncBoundary
        |      null match {
        |        case _ => foo2
        |      }
        |    case other => foo
        |  }
        |  class C extends Foo with Bar
        |}
        | """.stripMargin)
  }

  @Test def shadowing2(): Unit = {
    val result = run(
      """
        |import scala.async.run.late.{autoawait,lateasync}
        |object Test {
        |  trait Base; trait Foo[T <: Base] { @autoawait def func: Option[Foo[T]] = None }
        |  class Sub extends Base
        |  trait Bar extends Foo[Sub]
        |  def test: Any = test(new Bar {})
        |  @lateasync
        |  def test[T <: Base](foo: Foo[T]): Foo[T] = foo match {
        |    case foo: Bar =>
        |      val res = foo.func
        |      res match {
        |        case _ =>
        |      }
        |      foo
        |    case other => foo
        |  }
        |  test(new Bar {})
        |}
        | """.stripMargin)
  }

  @Test def patternAlternative(): Unit = {
    val result = wrapAndRun(
      """
        |  @autoawait def one = 1
        |
        |  @lateasync def test = {
        |    Option(true) match {
        |      case null | None => false
        |      case Some(v) => one; v
        |    }
        |  }
        | """.stripMargin)
  }

  @Test def patternAlternativeBothAnnotations(): Unit = {
    val result = wrapAndRun(
      """
        |import scala.async.run.late.{autoawait,lateasync}
        |object Test {
        | @autoawait def func1() = "hello"
        | @lateasync def func(a: Option[Boolean]) = a match {
        |    case null | None => func1 + " world"
        |    case _ => "okay"
        |  }
        |  def test: Any = func(None)
        |}
        | """.stripMargin)
  }

  @Test def shadowingRefinedTypes(): Unit = {
    val result = run(
      s"""
         |import scala.async.run.late.{autoawait,lateasync}
         |trait Base
         |class Sub extends Base
         |trait Foo[T <: Base] {
         |   @autoawait def func: Option[Foo[T]] = None
         |}
         |trait Bar extends Foo[Sub]
         |object Test {
         |   @lateasync def func[T <: Base](foo: Foo[T]): Foo[T] = foo match {   // the whole pattern match will be wrapped with async{ }
         |      case foo: Bar =>
         |         val res = foo.func  // will be rewritten into: await(foo.func)
         |         res match {
         |            case Some(v) => v   // this will report type mismtach
         |            case other => foo
         |         }
         |      case other => foo
         |   }
         |   def test: Any = { val b = new Bar{}; func(b) == b }
         |}""".stripMargin)
    assertEquals(true, result)
  }

  @Test def testMatchEndIssue(): Unit = {
    val result = run(
      """
        |import scala.async.run.late.{autoawait,lateasync}
        |sealed trait Subject
        |final class Principal(val name: String) extends Subject
        |object Principal {
        |  def unapply(p: Principal): Option[String] = Some(p.name)
        |}
        |object Test {
        |  @autoawait @lateasync
        |  def containsPrincipal(search: String, value: Subject): Boolean = value match {
        |    case Principal(name) if name == search => true
        |    case Principal(name) => containsPrincipal(search, value)
        |    case other => false
        |  }
        |
        |  @lateasync
        |  def test = containsPrincipal("test", new Principal("test"))
        |}
        | """.stripMargin)
  }

  @Test def testGenericTypeBoundaryIssue(): Unit = {
    val result = run(
      """

      import scala.async.run.late.{autoawait,lateasync}
      trait InstrumentOfValue
      trait Security[T <: InstrumentOfValue] extends InstrumentOfValue
      class Bound extends Security[Bound]
      class Futures extends Security[Futures]
      object TestGenericTypeBoundIssue {
        @autoawait @lateasync def processBound(bound: Bound): Unit = { println("process Bound") }
        @autoawait @lateasync def processFutures(futures: Futures): Unit = { println("process Futures") }
        @autoawait @lateasync def doStuff(sec: Security[_]): Unit = {
          sec match {
            case bound: Bound => processBound(bound)
            case futures: Futures => processFutures(futures)
            case _ => throw new Exception("Unknown Security type: " + sec)
          }
        }
      }
      object Test { @lateasync def test: Unit = TestGenericTypeBoundIssue.doStuff(new Bound) }
      """.stripMargin)
  }

  @Test def testReturnTupleIssue(): Unit = {
    val result = run(
      """
      import scala.async.run.late.{autoawait,lateasync}
      class TestReturnExprIssue(str: String) {
        @autoawait @lateasync def getTestValue = Some(42)
        @autoawait @lateasync def doStuff: Int = {
          val opt: Option[Int] = getTestValue  // here we have an async method invoke
          opt match {
            case Some(li) => li  // use the result somehow
            case None =>
          }
          42 // type mismatch;   found   : AnyVal   required: Int
        }
      }
      object Test { @lateasync def test: Unit = new TestReturnExprIssue("").doStuff }
      """.stripMargin)
  }


  @Test def testAfterRefchecksIssue(): Unit = {
    val result = run(
      """
      import scala.async.run.late.{autoawait,lateasync}
      trait Factory[T] { def create: T }
      sealed trait TimePoint
      class TimeLine[TP <: TimePoint](val tpInitial: Factory[TP]) {
        @autoawait @lateasync private[TimeLine] val tp: TP = tpInitial.create
        @autoawait @lateasync def timePoint: TP = tp
      }
      object Test {
        def test: Unit = ()
      }
      """)
  }

  @Test def testArrayIndexOutOfBoundIssue(): Unit = {
    val result = run(
      """
      import scala.async.run.late.{autoawait,lateasync}

      sealed trait Result
      case object A extends Result
      case object B extends Result
      case object C extends Result

      object Test {
        protected def doStuff(res: Result) = {
          class C {
             @autoawait def needCheck = false

             @lateasync def m = {
             if (needCheck) "NO"
              else {
                res match {
                  case A => 1
                  case _ => 2
                }
              }
            }
          }
        }


        @lateasync
        def test() = doStuff(B)
      }
      """)
  }

  def wrapAndRun(code: String): Any = {
    run(
      s"""
         |import scala.async.run.late.{autoawait,lateasync}
         |object Test {
         |  @lateasync
         |  def test: Any = {
         |    $code
         |  }
         |}
         | """.stripMargin)
  }


  @Test def testNegativeArraySizeException(): Unit = {
    val result = run(
      """
      import scala.async.run.late.{autoawait,lateasync}

      object Test {
        def foo(foo: Any, bar: Any) = ()
        @autoawait def getValue = 4.2
        @lateasync def func(f: Any) = {
          foo(f match { case _ if "".isEmpty => 2 }, getValue);
        }

        @lateasync
        def test() = func(4)
      }
      """)
  }

  @Test def testNegativeArraySizeExceptionFine1(): Unit = {
    val result = run(
      """
      import scala.async.run.late.{autoawait,lateasync}
      case class FixedFoo(foo: Int)
      class Foobar(val foo: Int, val bar: Double) {
        @autoawait @lateasync def getValue = 4.2
        @autoawait @lateasync def func(f: Any) = {
          new Foobar(foo = f match {
            case FixedFoo(x) => x
            case _ => 2
          },
          bar = getValue)
        }
      }
      object Test {
        @lateasync def test() = new Foobar(0, 0).func(4)
      }
      """)
  }

  @Test def testByNameOwner(): Unit = {
    val result = run(
      """
      import scala.async.run.late.{autoawait,lateasync}
      object Bleh {
        @autoawait @lateasync def asyncCall(): Int = 0
        def byName[T](fn: => T): T = fn
      }
      object Boffo {
        @autoawait @lateasync def jerk(): Unit = {
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
        @lateasync def test() = Boffo.jerk()
      }
      """)
  }

  @Test def testByNameOwner2(): Unit = {
    val result = run(
      """
      import scala.async.run.late.{autoawait,lateasync}
      object Bleh {
        @autoawait @lateasync def bleh = Bleh
        def byName[T](fn: => T): T = fn
      }
      object Boffo {
        @autoawait @lateasync def slob(): Unit = {
          val pointlessSymbolOwner =  {
              Bleh.bleh.byName {
                val whyDoHateMeToo = 1
                whyDoHateMeToo
              }
          }
        }
      }
      object Test {
        @lateasync def test() = Boffo.slob()
      }
      """)
  }

  private def createTempDir(): File = {
    val f = File.createTempFile("output", "")
    f.delete()
    f.mkdirs()
    f
  }

  def run(code: String): Any = {
    val out = createTempDir()
    try {
      val reporter = new StoreReporter
      val settings = new Settings(println(_))
      settings.outdir.value = out.getAbsolutePath
      settings.embeddedDefaults(getClass.getClassLoader)
      // settings.processArgumentString("-Xprint:patmat,postpatmat,jvm -nowarn")
      val isInSBT = !settings.classpath.isSetByUser
      if (isInSBT) settings.usejavacp.value = true
      val global = new Global(settings, reporter) {
        self =>

        object late extends {
          val global: self.type = self
        } with LatePlugin

        override protected def loadPlugins(): List[Plugin] = late :: Nil
      }
      import global._

      val run = new Run
      val source = newSourceFile(code)
      //    TreeInterrogation.withDebug {
      run.compileSources(source :: Nil)
      //    }
      Assert.assertTrue(reporter.infos.mkString("\n"), !reporter.hasErrors)
      val loader = new URLClassLoader(Seq(new File(settings.outdir.value).toURI.toURL), global.getClass.getClassLoader)
      val cls = loader.loadClass("Test")
      cls.getMethod("test").invoke(null)
    } finally {
      scala.reflect.io.Path.apply(out).deleteRecursively()
    }
  }
}

abstract class LatePlugin extends Plugin {

  import global._

  override val components: List[PluginComponent] = List(new PluginComponent with TypingTransformers {
    val global: LatePlugin.this.global.type = LatePlugin.this.global

    lazy val asyncIdSym = symbolOf[AsyncId.type]
    lazy val asyncSym = asyncIdSym.info.member(TermName("async"))
    lazy val awaitSym = asyncIdSym.info.member(TermName("await"))
    lazy val autoAwaitSym = symbolOf[autoawait]
    lazy val lateAsyncSym = symbolOf[lateasync]

    def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = {
        super.transform(tree) match {
          case ap@Apply(fun, args) if fun.symbol.hasAnnotation(autoAwaitSym) =>
            localTyper.typed(Apply(TypeApply(gen.mkAttributedRef(asyncIdSym.typeOfThis, awaitSym), TypeTree(ap.tpe) :: Nil), ap :: Nil))
          case sel@Select(fun, _) if sel.symbol.hasAnnotation(autoAwaitSym) && !(tree.tpe.isInstanceOf[MethodTypeApi] || tree.tpe.isInstanceOf[PolyTypeApi]) =>
            localTyper.typed(Apply(TypeApply(gen.mkAttributedRef(asyncIdSym.typeOfThis, awaitSym), TypeTree(sel.tpe) :: Nil), sel :: Nil))
          case dd: DefDef if dd.symbol.hasAnnotation(lateAsyncSym) => atOwner(dd.symbol) {
            deriveDefDef(dd) { rhs: Tree =>
              val invoke = Apply(TypeApply(gen.mkAttributedRef(asyncIdSym.typeOfThis, asyncSym), TypeTree(rhs.tpe) :: Nil), List(rhs))
              localTyper.typed(atPos(dd.pos)(invoke))
            }
          }
          case vd: ValDef if vd.symbol.hasAnnotation(lateAsyncSym) => atOwner(vd.symbol) {
            deriveValDef(vd) { rhs: Tree =>
              val invoke = Apply(TypeApply(gen.mkAttributedRef(asyncIdSym.typeOfThis, asyncSym), TypeTree(rhs.tpe) :: Nil), List(rhs))
              localTyper.typed(atPos(vd.pos)(invoke))
            }
          }
          case vd: ValDef =>
            vd
          case x => x
        }
      }
    }

    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        val translated = newTransformer(unit).transformUnit(unit)
        //println(show(unit.body))
        translated
      }
    }

    override val runsAfter: List[String] = "refchecks" :: Nil
    override val phaseName: String = "postpatmat"

  })
  override val description: String = "postpatmat"
  override val name: String = "postpatmat"
}

// Methods with this annotation are translated to having the RHS wrapped in `AsyncId.async { <original RHS> }`
@field
final class lateasync extends StaticAnnotation

// Calls to methods with this annotation are translated to `AsyncId.await(<call>)`
@getter
final class autoawait extends StaticAnnotation
