package scala.tools.nsc

import org.junit.Test

import scala.reflect.internal.util.{BatchSourceFile, SourceFile}

class DeterminismTest {
  private val tester = new DeterminismTester
  import tester.test

  @Test def testLambdaLift(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |package demo
          |
          |class a {
          |  def x = {
          |    def local = "x"
          |  }
          |  def y = {
          |    def local = "y"
          |  }
          |}
          |
      """.stripMargin),
      source("b.scala",
      """
        |package demo
        |
        |class b {
        |  def test: Unit = {
        |    new a().y
        |  }
        |}
      """.stripMargin)

    )
    test(List(code))
  }
  @Test def testTyperFreshName(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |package demo
          |
          |class a {
          |  def x = {
          |    { case x if "".isEmpty => "" }: PartialFunction[Any, Any]
          |  }
          |  def y = {
          |    { case x if "".isEmpty => "" }: PartialFunction[Any, Any]
          |  }
          |}
          |
      """.stripMargin),
      source("b.scala",
      """
        |package demo
        |
        |class b {
        |  def test: Unit = {
        |    new a().y
        |  }
        |}
      """.stripMargin)

    )
    test(List(code))
  }

  @Test def testReify(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |package demo
          |
          |import language.experimental.macros
          |import scala.reflect.macros.blackbox.Context
          |
          |class a {
          |  def x(c: Context) = {
          |    import c.universe._
          |    reify { type T = Option[_]; () }.tree
          |  }
          |  def y(c: Context) = {
          |    import c.universe._
          |    reify { type T = Option[_]; () }.tree
          |  }
          |}
          |
      """.stripMargin),
      source("b.scala",
      """
        |package demo
        |
        |class b {
        |  def test: Unit = {
        |    new a().y(null)
        |  }
        |}
      """.stripMargin)

    )
    test(List(code))
  }

  @Test def testMacroFreshName(): Unit = {
    val macroCode = source("macro.scala",
        """
          |package demo
          |
          |import language.experimental.macros
          |import scala.reflect.macros.blackbox.Context
          |
          |object Macro {
          |  def impl(c: Context): c.Tree = {
          |    import c.universe._
          |    val name = c.freshName("foo")
          |    Block(ValDef(NoMods, TermName(name), tq"_root_.scala.Int", Literal(Constant(0))) :: Nil, Ident(name))
          |  }
          |  def m: Unit = macro impl
          |}
          |
      """.stripMargin)
    def code = List(
      source("a.scala",
      """
        |package demo
        |
        |class a {
        |  def test: Unit = {
        |    Macro.m
        |  }
        |}
      """.stripMargin),
        source("b.scala",
      """
        |package demo
        |
        |class b {
        |  def test: Unit = {
        |    Macro.m
        |  }
        |}
      """.stripMargin)

    )
    test(List(List(macroCode), code))
  }


  @Test def testRefinementTypeOverride(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |class Global
          |trait Analyzer extends StdAttachments {
          |  val global: Global
          |}
          |trait Context {
          |  val universe: Global
          |}
          |
          |trait StdAttachments {
          |  self: Analyzer =>
          |
          |  type UnaffiliatedMacroContext = Context
          |  type MacroContext = UnaffiliatedMacroContext { val universe: self.global.type }
          |}
          |
      """.stripMargin),
      source("b.scala",
        """
          |class Macros {
          |    self: Analyzer =>
          |  def foo = List.apply[MacroContext]()
          |}
          |
        """.stripMargin)
    )
    test(List(code))
  }

  @Test def testAnnotations1(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          |class Annot1(s: String) extends scala.annotation.StaticAnnotation
          |class Annot2(s: Class[_]) extends scala.annotation.StaticAnnotation
          |
      """.stripMargin),
      source("b.scala",
        """
          |@Annot1("foo")
          |@Annot2(classOf[AnyRef])
          |class Test
        """.stripMargin)
    )
    test(List(code))
  }

  @Test def testAnnotationsJava(): Unit = {
    def code = List[SourceFile](
      source("Annot1.java",
        """
          |import java.lang.annotation.*;
          |@Retention(RetentionPolicy.RUNTIME)
          |@Target(ElementType.TYPE)
          |@Inherited
          |@interface Annot1 { String value() default ""; }
          |
          |@Retention(RetentionPolicy.RUNTIME)
          |@Target(ElementType.TYPE)
          |@Inherited
          |@interface Annot2 { Class value(); }
          |
      """.stripMargin),
      source("b.scala",
        """
          |@Annot1("foo") @Annot2(classOf[AnyRef]) class Test
        """.stripMargin)
    )
    test(List(code))
  }

  @Test def testAnnotationsJavaRepeatable(): Unit = {
    val javaAnnots = source("Annot1.java",
      """
        |import java.lang.annotation.*;
        |@Repeatable(Annot1.Container.class)
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.TYPE)
        |@interface Annot1 { String value() default "";
        |
        |    @Retention(RetentionPolicy.RUNTIME)
        |    @Target(ElementType.TYPE)
        |    public static @interface Container {
        |        Annot1[] value();
        |    }
        |}
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.TYPE)
        |@Inherited
        |@interface Annot2 { Class value(); }
      """.stripMargin)
    def code =
      List(source("dummy.scala", ""), source("b.scala",
        """
          |@Annot1("foo") @Annot2(classOf[String]) @Annot1("bar") class Test
        """.stripMargin)
    )
    test(List(javaAnnots) :: code :: Nil)
  }

  @Test def testPackedType(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          | class C {
          |   def foo = { object A; object B; object C; object D; object E; object F; def foo[A](a: A) = (a, a); foo((A, B, C, D, E))}
          | }
          |
      """.stripMargin)
    )
    test(List(code))
  }

  @Test def testSyntheticModuleLocationInDecls(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          | object A {
          |   class C(val a: Any) extends AnyVal
          |   implicit class I1(a: String)
          |   implicit class IV(val a: String) extends AnyVal
          |   case class CC()
          | }
          |
      """.stripMargin),
      source("b.scala",
        """
          | object B {
          |   // Order here reversed from definition order in A
          |   A.CC()
          |   A.IV("")
          |   A.I1("")
          |   new A.C("")
          | }
          |
      """.stripMargin)
    )
    test(List(code))
  }

  @Test def testAsync(): Unit = {
    def code = List[SourceFile](
      source("a.scala",
        """
          | object A {
          |   import scala.tools.nsc.OptionAwait.{optionally, value}
          |   def test = optionally {
          |      if (value(Some(true))) {
          |        var x = ""
          |        if (value(Some(false))) {
          |          value(Some(x)) + value(Some(2))
          |        }
          |      }
          |   }
          | }
          |
      """.stripMargin)
    )
    test(List(code))
  }

  @Test def testReferenceToInnerClassMadeNonPrivate(): Unit = {
    def code = List[SourceFile](
      source("t.scala",
             """
               | trait T {
               |   private class Inner
               |   class OtherInner { new Inner } // triggers makeNotPrivate of Inner
               |   private val v: Option[Inner] = None
               | }
        """.stripMargin),
      source("c.scala","""class C extends T""")
      )
    test(List(code))
  }

  def source(name: String, code: String): SourceFile = new BatchSourceFile(name, code)
}



import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object OptionAwait {
  def optionally[T](body: T): Option[T] = macro impl
  @compileTimeOnly("[async] `value` must be enclosed in `optionally`")
  def value[T](option: Option[T]): T = ???
  def impl(c: blackbox.Context)(body: c.Tree): c.Tree = {
    import c.universe._
    val awaitSym = typeOf[OptionAwait.type].decl(TermName("value"))
    def mark(t: DefDef): Tree = c.internal.markForAsyncTransform(c.internal.enclosingOwner, t, awaitSym, Map.empty)
    val name = TypeName("stateMachine$async")
    q"""
      final class $name extends _root_.scala.tools.nsc.OptionStateMachine {
        ${mark(q"""override def apply(tr$$async: _root_.scala.Option[_root_.scala.AnyRef]) = ${body}""")}
      }
      new $name().start().asInstanceOf[${c.macroApplication.tpe}]
    """
  }
}

trait AsyncStateMachine[F, R] {
  /** Assign `i` to the state variable */
  protected def state_=(i: Int): Unit
  /** Retrieve the current value of the state variable */
  protected def state: Int
  /** Complete the state machine with the given failure. */
  protected def completeFailure(t: Throwable): Unit
  /** Complete the state machine with the given value. */
  protected def completeSuccess(value: AnyRef): Unit
  /** Register the state machine as a completion callback of the given future. */
  protected def onComplete(f: F): Unit
  /** Extract the result of the given future if it is complete, or `null` if it is incomplete. */
  protected def getCompleted(f: F): R
  /**
   * Extract the success value of the given future. If the state machine detects a failure it may
   * complete the async block and return `this` as a sentinel value to indicate that the caller
   * (the state machine dispatch loop) should immediately exit.
   */
  protected def tryGet(tr: R): AnyRef
}


abstract class OptionStateMachine extends AsyncStateMachine[Option[AnyRef], Option[AnyRef]] {
  var result$async: Option[AnyRef] = _

  // FSM translated method
  def apply(tr$async: Option[AnyRef]): Unit

  // Required methods
  private[this] var state$async: Int = 0
  protected def state: Int = state$async
  protected def state_=(s: Int): Unit = state$async = s
  protected def completeFailure(t: Throwable): Unit = throw t
  protected def completeSuccess(value: AnyRef): Unit = result$async = Some(value)
  protected def onComplete(f: Option[AnyRef]): Unit = ???
  protected def getCompleted(f: Option[AnyRef]): Option[AnyRef] = {
    f
  }
  protected def tryGet(tr: Option[AnyRef]): AnyRef = tr match {
    case Some(value) =>
      value.asInstanceOf[AnyRef]
    case None =>
      result$async = None
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): Option[AnyRef] = {
    apply(None)
    result$async
  }
}

