package scala.reflect.internal

import org.junit.Test
import org.junit.Assert._
import scala.tools.reflect._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

object PrinterHelper {
  val toolbox = cm.mkToolBox()

  import scala.reflect.internal.Chars._
  private def normalizeEOL(resultCode: String) =
    resultCode.linesIterator mkString s"$LF"

  def assertResultCode(code: String)(parsedCode: String = "", typedCode: String = "", wrap: Boolean = false, printRoot: Boolean = false) = {
    def toolboxTree(tree: => Tree) = try {
        tree
      } catch {
        case e:scala.tools.reflect.ToolBoxError => throw new Exception(e.getMessage + ": " + code, e)
      }

    def wrapCode(source: String) = {
      val context = sm"""
      |trait PrintersContext {
      |  class baz extends scala.annotation.Annotation with scala.annotation.StaticAnnotation;
      |  class foo1[A, B] extends scala.annotation.Annotation with scala.annotation.StaticAnnotation;
      |  class foo2[A, B](a: scala.Int)(b: scala.Int) extends scala.annotation.Annotation with scala.annotation.StaticAnnotation;
      |  class foo3[Af, Bf](a: scala.Int)(b: scala.Float, c: PrintersContext.this.foo1[Af, Bf]) extends scala.annotation.Annotation with scala.annotation.StaticAnnotation;
      |  trait A1;
      |  trait B1;
      |${source.trim.linesIterator map {"  " + _} mkString s"$LF"}
      |}"""

      if (wrap) context.trim() else source.trim
    }

    val parsedTree = toolboxTree(toolbox.parse(wrapCode(code)))
    if (!parsedCode.isEmpty())
      assertEquals("using toolbox parser" + LF, wrapCode(parsedCode), normalizeEOL(showCode(parsedTree)))
    if (!typedCode.isEmpty()) {
      val typedTree = toolboxTree(toolbox.typecheck(parsedTree))
      assertEquals("using toolbox typechecker" + LF, wrapCode(typedCode), normalizeEOL(showCode(typedTree, printRootPkg = printRoot)))
    }
  }

  def assertTreeCode(tree: Tree, typecheck: Boolean = false)(code: String) = {
    if (typecheck) {
      assertEquals("using quasiquote or given tree (typechecked)"+LF, code.trim, normalizeEOL(showCode(toolbox.typecheck(tree))))
    } else {
      assertEquals("using quasiquote or given tree"+LF, code.trim, normalizeEOL(showCode(tree)))
    }
  }

  def assertPrintedCode(source: String, checkTypedTree: Boolean = true, wrapCode: Boolean = false) = {
    if (checkTypedTree)
      assertResultCode(source)(source, source, wrapCode)
    else assertResultCode(source)(parsedCode = source, wrap = wrapCode)
  }

  implicit class StrContextStripMarginOps(val stringContext: StringContext) extends util.StripMarginInterpolator
}

import PrinterHelper._

@RunWith(classOf[JUnit4])
class BasePrintTest {
  @Test def testIdent(): Unit = assertTreeCode(Ident(TermName("*")))("*")

  @Test def testConstant1(): Unit = assertTreeCode(Literal(Constant("*")))("\"*\"")

  @Test def testConstant2(): Unit = assertTreeCode(Literal(Constant(42)))("42")

  @Test def testConstantFloat(): Unit = assertTreeCode(Literal(Constant(42f)))("42.0F")

  @Test def testConstantDouble(): Unit = assertTreeCode(Literal(Constant(42d)))("42.0")

  @Test def testConstantLong(): Unit = assertTreeCode(Literal(Constant(42L)))("42L")

  @Test def testConstantNull(): Unit = assertTreeCode(Literal(Constant(null)))("null")

  val sq  = "\""
  val tq  = "\"" * 3
  val teq = "\"\"\\\""

  @Test def testConstantMultiline(): Unit = assertTreeCode(Literal(Constant("hello\nworld")))(s"${tq}hello\nworld${tq}")

  @Test def testConstantFormfeed(): Unit = assertTreeCode(Literal(Constant("hello\fworld")))(s"${sq}hello\\fworld${sq}")

  @Test def testConstantControl(): Unit = assertTreeCode(Literal(Constant("hello\u0003world")))(s"${sq}hello\\u0003world${sq}")

  // ISOControl is 0-1F, 7F-9F
  @Test def testConstantABControl(): Unit = assertTreeCode(Literal(Constant("hello\u009fworld")))(s"${sq}hello\\u009Fworld${sq}")

  @Test def testConstantFormfeedChar(): Unit = assertTreeCode(Literal(Constant('\f')))("'\\f'")

  @Test def testConstantControlChar(): Unit = assertTreeCode(Literal(Constant(3.toChar)))("'\\u0003'")

  @Test def testConstantEmbeddedTriple(): Unit = assertTreeCode(Literal(Constant(s"${tq}hello${tq}\nworld")))(s"${tq}${teq}hello${teq}\nworld${tq}")

  @Test def testOpExpr(): Unit = assertPrintedCode("(5).+(4)", checkTypedTree = false)

  @Test def testName1(): Unit = assertPrintedCode("class test")

  @Test def testName2(): Unit = assertPrintedCode("class *")

  @Test def testName4(): Unit = assertPrintedCode("class `a*`")

  @Test def testName5(): Unit = assertPrintedCode("val :::: = 1")

  @Test def testName6(): Unit = assertPrintedCode("val `::::t` = 1")

  @Test def testName7(): Unit = assertPrintedCode("""class \/""")

  @Test def testName8(): Unit = assertPrintedCode("""class \\\\""")

  @Test def testName9(): Unit = assertPrintedCode("""class test_\/""")

  @Test def testName10(): Unit = assertPrintedCode("""class `*_*`""")

  @Test def testName11(): Unit = assertPrintedCode("""class `a_*`""")

  @Test def testName12(): Unit = assertPrintedCode("""class `*_a`""")

  @Test def testName13(): Unit = assertPrintedCode("""class a_a""")

  @Test def testName14(): Unit = assertPrintedCode("val x$11 = 5")

  @Test def testName15(): Unit = assertPrintedCode("class `[]`")

  @Test def testName16(): Unit = assertPrintedCode("class `()`")

  @Test def testName17(): Unit = assertPrintedCode("class `{}`")

  @Test def testName18(): Unit = assertPrintedCode("class <>")

  @Test def testName19(): Unit = assertPrintedCode("""class `class`""")

  @Test def testName20(): Unit = assertPrintedCode("""class `test name`""")

  @Test def testName21(): Unit = assertPrintedCode("""class `test.name`""")

  @Test def testName22(): Unit = assertEquals("val `some-val`: Int = 1", showCode(q"""val ${TermName("some-val")}: Int = 1"""))

  @Test def testName23(): Unit = assertEquals("val `some+`: String = \"foo\"", showCode(q"""val ${TermName("some+")}: String = "foo""""))

  @Test def testIfExpr1(): Unit = assertResultCode(code = sm"""
    |val a = 1
    |if (a > 1)
    |  a: Int
    |else
    |  (a.toString): String
    """)(
    parsedCode = sm"""
    |val a = 1;
    |if (a.>(1))
    |  ((a): Int)
    |else
    |  ((a.toString): String)""",
    typedCode=sm"""
    |val a = 1;
    |if (PrintersContext.this.a.>(1))
    |  ((PrintersContext.this.a): scala.Int)
    |else
    |  ((PrintersContext.this.a.toString()): scala.Predef.String)
    """, wrap = true)

  @Test def testIfExpr2(): Unit = assertPrintedCode(sm"""
    |class A {
    |  (if (true)
    |  {
    |    false;
    |    ()
    |  }
    |else
    |  {
    |    true;
    |    ()
    |  }).toString()
    |}""")

  @Test def testIfExpr3(): Unit = assertPrintedCode(sm"""
    |class A {
    |  (if (true)
    |  {
    |    false;
    |    ()
    |  }
    |else
    |  {
    |    true;
    |    ()
    |  }).toString().hashCode()
    |}""")

  //val x = true && true && false.!
  @Test def testBooleanExpr1(): Unit = assertPrintedCode("val x = true.&&(true).&&(false.`unary_!`)", checkTypedTree = false)

  //val x = true && !(true && false)
  @Test def testBooleanExpr2(): Unit = assertPrintedCode("val x = true.&&(true.&&(false).`unary_!`)", checkTypedTree = false)

  @Test def testNewExpr1(): Unit = assertResultCode(
    code = sm"""
    |class foo
    |new foo()
    |""")(
    parsedCode = sm"""
    |class foo;
    |new foo()""",
    typedCode = sm"""
    |class foo;
    |new PrintersContext.this.foo()
    |""",
    wrap = true)

  @Test def testNewExpr2(): Unit = assertResultCode(
    code = sm"""
    |class foo
    |new foo { "test" }
    |""")(
    parsedCode = sm"""
    |class foo;
    |{
    |  final class $$anon extends foo {
    |    "test"
    |  };
    |  new $$anon()
    |}""",
    typedCode = sm"""
    |class foo;
    |{
    |  final class $$anon extends PrintersContext.this.foo {
    |    "test"
    |  };
    |  new $$anon()
    |}""",
    wrap = true)

  @Test def testNewExpr3(): Unit = assertPrintedCode(sm"""
    |{
    |  class foo[t];
    |  new foo[scala.Int]()
    |}""")

  @Test def testNewExpr4(): Unit = assertPrintedCode(sm"""
    |{
    |  class foo(x: scala.Int);
    |  val x = 5;
    |  new foo(x)
    |}""")

  @Test def testNewExpr5(): Unit = assertPrintedCode(sm"""
    |{
    |  class foo[t](x: scala.Int);
    |  val x = 5;
    |  new foo[scala.Predef.String](x)
    |}""")

  //new foo[t](x) { () }
  @Test def testNewExpr6(): Unit = assertResultCode(
    code = sm"""
    |class foo[t](x: Int)
    |new foo[String](3) { () }
    |""")(
    parsedCode = sm"""
    |{
    |  class foo[t](x: Int);
    |  {
    |    final class $$anon extends foo[String](3) {
    |      ()
    |    };
    |    new $$anon()
    |  }
    |}""",
    typedCode = sm"""
    |{
    |  class foo[t](x: scala.Int);
    |  {
    |    final class $$anon extends foo[scala.Predef.String](3) {
    |      ()
    |    };
    |    new $$anon()
    |  }
    |}""")

  //new foo with bar
  @Test def testNewExpr7(): Unit = assertPrintedCode(sm"""
    |{
    |  trait foo;
    |  trait bar;
    |  {
    |    final class $$anon extends foo with bar;
    |    new $$anon()
    |  }
    |}""")

  //new { anonymous }
  @Test def testNewExpr8(): Unit = assertPrintedCode(sm"""
    |{
    |  final class $$anon {
    |    5
    |  };
    |  new $$anon()
    |}""")

  //new { val early = 1 } with Parent[Int] { body }
  @Test def testNewExpr9(): Unit = assertPrintedCode(sm"""
    |{
    |  class Parent[t];
    |  {
    |    final class $$anon extends {
    |      val early = 1
    |    } with Parent[scala.Int] {
    |      "testNewExpr"
    |    };
    |    new $$anon()
    |  }
    |}""")

  //new Foo { self => }
  @Test def testNewExpr10(): Unit = assertPrintedCode(sm"""
    |{
    |  class Foo;
    |  {
    |    final class $$anon extends Foo { self =>
    |      
    |    };
    |    new $$anon()
    |  }
    |}""")

  @Test def testReturn(): Unit = assertPrintedCode("def test: scala.Int = return 42")

  @Test def testFunc1(): Unit = assertResultCode(
    code = "List(1, 2, 3).map((i: Int) => i - 1)")(
    parsedCode = "List(1, 2, 3).map(((i: Int) => i.-(1)))",
    typedCode = sm"scala.`package`.List.apply[Int](1, 2, 3).map[Int](((i: scala.Int) => i.-(1)))")

  @Test def testFunc2(): Unit = assertResultCode(
    code = "val sum: Seq[Int] => Int = _ reduceLeft (_+_)")(
    parsedCode = "val sum: _root_.scala.Function1[Seq[Int], Int] = ((x$1) => x$1.reduceLeft(((x$2, x$3) => x$2.+(x$3))))",
    typedCode = "val sum: scala.Function1[scala.`package`.Seq[scala.Int], scala.Int] = ((x$1: Seq[Int]) => x$1.reduceLeft[Int](((x$2: Int, x$3: Int) => x$2.+(x$3))))")

  @Test def testFunc3(): Unit = assertResultCode(
    code = "List(1, 2, 3) map (_ - 1)")(
    parsedCode = "List(1, 2, 3).map(((x$1) => x$1.-(1))) ",
    typedCode = "scala.`package`.List.apply[Int](1, 2, 3).map[Int](((x$1: Int) => x$1.-(1)))")

  @Test def testFunc4(): Unit = assertResultCode(
    code = "val x: String => Int = ((str: String) => 1)")(
    parsedCode = "val x: _root_.scala.Function1[String, Int] = ((str: String) => 1)",
    typedCode = " val x: _root_.scala.Function1[_root_.scala.Predef.String, _root_.scala.Int] = ((str: _root_.scala.Predef.String) => 1)", printRoot = true)

  @Test def testAssign1(): Unit = assertPrintedCode("(f.v = 5).toString", checkTypedTree = false)

  @Test def testAssign2(): Unit = assertPrintedCode("(f.v = 5)(2)", checkTypedTree = false)

  @Test def testImport1(): Unit = assertPrintedCode("import scala.collection.mutable")

  @Test def testImport2(): Unit = assertPrintedCode("import java.lang.{String=>Str}")

  @Test def testImport3(): Unit = assertPrintedCode("import java.lang.{String=>Str, Object=>_, _}")

  @Test def testImport4(): Unit = assertPrintedCode("import scala.collection._")
}

@RunWith(classOf[JUnit4])
class ClassPrintTest {
  @Test def testClass(): Unit = assertPrintedCode("class *")

  @Test def testClassWithBody(): Unit = assertPrintedCode(sm"""
    |class X {
    |  def y = "test"
    |}""")

  @Test def testClassConstructorModifiers(): Unit = assertPrintedCode("class X private (x: scala.Int)")

  @Test def testClassConstructorModifierVisibility(): Unit = assertPrintedCode(sm"""
    |object A {
    |  class X protected[A] (x: scala.Int)
    |}""")

  @Test def testClassWithPublicParams(): Unit = assertPrintedCode("class X(val x: scala.Int, val s: scala.Predef.String)")

  @Test def testClassWithParams1(): Unit = assertPrintedCode("class X(x: scala.Int, s: scala.Predef.String)")

  @Test def testClassWithParams2(): Unit = assertPrintedCode("class X(@test x: Int, s: String)", checkTypedTree = false)

  @Test def testClassWithParams3(): Unit = assertPrintedCode("class X(implicit x: Int, s: String)", checkTypedTree = false)

  @Test def testClassWithParams4(): Unit = assertPrintedCode("class X(implicit @unchecked x: Int, s: String)", checkTypedTree = false)

  @Test def testClassWithParams5(): Unit = assertPrintedCode(sm"""
    |{
    |  class Y {
    |    val x = 5
    |  };
    |  class X(override private[this] val x: scala.Int, s: scala.Predef.String) extends Y;
    |  ()
    |}""")

  @Test def testClassWithParams6(): Unit = assertPrintedCode("class X(@test1 override private[this] val x: Int, @test2(param1 = 7) s: String) extends Y", checkTypedTree = false)

  @Test def testClassWithParams7(): Unit = assertPrintedCode("class X protected (val x: scala.Int, val s: scala.Predef.String)")

  @Test def testClassWithParams8(): Unit = assertPrintedCode("class X(var x: scala.Int)")

  @Test def testClassWithParams9(): Unit = assertPrintedCode("def test(x: scala.Int*) = 5")

  @Test def testClassWithByNameParam(): Unit = assertPrintedCode("class X(x: => scala.Int)")

  @Test def testClassWithDefault(): Unit = assertPrintedCode(sm"""
    |{
    |  class X(var x: scala.Int = 5);
    |  ()
    |}""")

  @Test def testClassWithParams10(): Unit = assertPrintedCode("class X(protected[zzz] var x: Int)", checkTypedTree = false)

  @Test def testClassWithParams11(): Unit = assertPrintedCode(sm"""
    |{
    |  class F(x: scala.Int);
    |  trait E {
    |    var x: scala.Int
    |  };
    |  class X(override var x: scala.Int = 5) extends F(x) with E;
    |  ()
    |}""")

  @Test def testClassWithParams12(): Unit = assertPrintedCode("class X(val y: scala.Int)()(var z: scala.Double)")

  @Test def testClassWithImplicitParams(): Unit = assertPrintedCode("class X(var i: scala.Int)(implicit val d: scala.Double, var f: scala.Float)")

  @Test def testClassWithEarly(): Unit =
    assertPrintedCode(sm"""
    |class X(var i: scala.Int) extends {
    |  val a = i;
    |  type B
    |} with scala.`package`.Serializable""")

  @Test def testClassWithThrow1(): Unit = assertPrintedCode(sm"""
    |class Throw1 {
    |  throw new scala.`package`.Exception("exception!")
    |}""")

  @Test def testClassWithThrow2(): Unit = assertPrintedCode(sm"""
    |class Throw2 {
    |  var msg = "   ";
    |  val e = new scala.`package`.Exception(Throw2.this.msg);
    |  throw Throw2.this.e
    |}""")

  @Test def testClassWithAssignmentWithTuple1(): Unit = assertResultCode(sm"""
    |class Test {
    |  val (a, b) = (1, 2)
    |}""")(
    parsedCode = sm"""
    |class Test {
    |  private[this] val x$$1 = (scala.Tuple2(1, 2): @scala.unchecked) match {
    |    case scala.Tuple2((a @ _), (b @ _)) => scala.Tuple2(a, b)
    |  };
    |  val a = x$$1._1;
    |  val b = x$$1._2
    |}""",
    typedCode = sm"""
    |class Test {
    |  private[this] val x$$1 = (scala.Tuple2.apply[Int, Int](1, 2): @scala.unchecked) match {
    |    case scala.Tuple2((a @ _), (b @ _)) => scala.Tuple2.apply[Int, Int](a, b)
    |  };
    |  val a = Test.this.x$$1._1;
    |  val b = Test.this.x$$1._2
    |}""")

  @Test def testClassWithAssignmentWithTuple2(): Unit = assertResultCode(
    code = sm"""
    |class Test {
    |  val (a, b) = (1).->(2)
    |}""")(
    parsedCode = sm"""
    |class Test {
    |  private[this] val x$$1 = ((1).->(2): @scala.unchecked) match {
    |    case scala.Tuple2((a @ _), (b @ _)) => scala.Tuple2(a, b)
    |  };
    |  val a = x$$1._1;
    |  val b = x$$1._2
    |}""",
    typedCode = sm"""
    |class Test {
    |  private[this] val x$$1 = (scala.Predef.ArrowAssoc[Int](1).->[Int](2): @scala.unchecked) match {
    |    case scala.Tuple2((a @ _), (b @ _)) => scala.Tuple2.apply[Int, Int](a, b)
    |  };
    |  val a = Test.this.x$$1._1;
    |  val b = Test.this.x$$1._2
    |}""")

  /*
    class Test {
      val List(one, three, five) = List(1,3,5)
    }
  */
  @Test def testClassWithPatternMatchInAssignment(): Unit = assertPrintedCode(sm"""
    |class Test {
    |  private[this] val x$$1 = (scala.collection.immutable.List.apply[scala.Int](1, 3, 5): @scala.unchecked) match {
    |    case scala.collection.immutable.List((one @ _), (three @ _), (five @ _)) => scala.Tuple3.apply[scala.Int, scala.Int, scala.Int](one, three, five)
    |  };
    |  val one = Test.this.x$$1._1;
    |  val three = Test.this.x$$1._2;
    |  val five = Test.this.x$$1._3
    |}""")

  //class A(l: List[_])
  @Test def testClassWithExistentialParameter1(): Unit = assertPrintedCode(sm"""
    |class Test(l: (scala.`package`.List[_$$1] forSome { 
    |  type _$$1
    |}))""")

  @Test def testClassWithExistentialParameter2(): Unit = assertPrintedCode(sm"""
    |class B(l: (scala.`package`.List[T] forSome { 
    |  type T
    |}))""")

  @Test def testClassWithCompoundTypeTree(): Unit = assertPrintedCode(sm"""
    |{
    |  trait A;
    |  trait B;
    |  abstract class C(val a: A with B) {
    |    def method(x: A with B with C {
    |      val x: scala.Float
    |    }): A with B
    |  };
    |  ()
    |}""")

  @Test def testClassWithSelectFromTypeTree(): Unit = assertPrintedCode(sm"""
    |{
    |  trait A {
    |    type T
    |  };
    |  class B(t: (A)#T);
    |  ()
    |}""")

  @Test def testImplicitClass(): Unit = assertPrintedCode(sm"""
    |{
    |  implicit class X(protected[this] var x: scala.Int);
    |  ()
    |}""",
    checkTypedTree = true)

  @Test def testAbstractClass(): Unit = assertPrintedCode("abstract class X(protected[this] var x: scala.Int)")

  @Test def testCaseClassWithParams1(): Unit = assertPrintedCode(sm"""
    |{
    |  case class X(x: scala.Int, s: scala.Predef.String);
    |  ()
    |}""")

  @Test def testCaseClassWithParams2(): Unit = assertPrintedCode(sm"""
    |{
    |  case class X(protected val x: scala.Int, s: scala.Predef.String);
    |  ()
    |}""")

  @Test def testCaseClassWithParams3(): Unit = assertPrintedCode(sm"""
    |{
    |  case class X()(implicit x: scala.Int, s: scala.Predef.String);
    |  ()
    |}""")

  @Test def testCaseClassWithParams4(): Unit = assertPrintedCode(sm"""
    |{
    |  trait V {
    |    val x: scala.Int
    |  };
    |  case class X(override val x: scala.Int, s: scala.Predef.String) extends scala.`package`.Cloneable;
    |  ()
    |}""")

  @Test def testCaseClassWithBody(): Unit = assertPrintedCode(sm"""
    |{
    |  case class X() {
    |    def y = "test"
    |  };
    |  ()
    |}""")

  @Test def testLocalClass(): Unit = assertPrintedCode(sm"""
    |def test = {
    |  class X(var a: scala.Int) {
    |    def y = "test"
    |  };
    |  new X(5)
    |}""")

  @Test def testLocalCaseClass(): Unit = assertPrintedCode(sm"""
    |def test = {
    |  case class X(var a: scala.Int) {
    |    def y = "test"
    |  };
    |  new X(5)
    |}""")

  @Test def testSuperInClass(): Unit = assertPrintedCode(sm"""
    |{
    |  trait Root {
    |    def r = "Root"
    |  };
    |  class X extends Root {
    |    def superX = super.r
    |  };
    |  class Y extends X with Root {
    |    class Inner {
    |      val myY = Y.super.r
    |    };
    |    def fromX = super[X].r;
    |    def fromRoot = super[Root].r
    |  };
    |  ()
    |}""")

  @Test def testThisInClass(): Unit = assertPrintedCode(sm"""
    |class Outer {
    |  class Inner {
    |    val outer = Outer.this
    |  };
    |  val self = this
    |}""")

  @Test def testCaseClassWithParamsAndBody(): Unit = assertPrintedCode(sm"""
    |{
    |  case class X(var x: scala.Int, var s: scala.Predef.String) {
    |    def y = "test"
    |  };
    |  ()
    |}""")

  @Test def testObject(): Unit = assertPrintedCode("object *")

  @Test def testObjectWithBody(): Unit = assertPrintedCode(sm"""
    |object X {
    |  def y = "test"
    |}""")

  @Test def testObjectWithEarly1(): Unit = assertPrintedCode(sm"""
    |object X extends {
    |  val early: scala.Int = 42
    |} with scala.`package`.Serializable""")

  @Test def testObjectWithEarly2(): Unit = assertPrintedCode(sm"""
    |object X extends {
    |  val early: scala.Int = 42;
    |  type EarlyT = scala.Predef.String
    |} with scala.`package`.Serializable""")

  @Test def testObjectWithSelf(): Unit = assertPrintedCode(sm"""
    |object Foo extends scala.`package`.Serializable { self =>
    |  42
    |}""")

  @Test def testObjectInh(): Unit = assertPrintedCode(sm"""
    |trait Y {
    |  private[Y] object X extends scala.`package`.Serializable with scala.`package`.Cloneable
    |}""")

  @Test def testObjectWithPatternMatch1(): Unit = assertPrintedCode(sm"""
    |object PM1 {
    |  scala.collection.immutable.List.apply[scala.Int](1, 2) match {
    |    case (i @ _) => i
    |  }
    |}""")

  @Test def testObjectWithPatternMatch2(): Unit = assertResultCode(
    code = sm"""
    |object PM2 {
    |  List(1, 2).map {
    |    case i if i > 5 => i
    |  }
    |}""")(
    parsedCode = sm"""
    |object PM2 {
    |  List(1, 2).map({
    |    case (i @ _) if i.>(5) => i
    |  })
    |}""")
    /*
    typedCode = sm"""
    |object PM2 {
    |  scala.collection.immutable.List.apply(1, 2).map(((x0$$1) => x0$$1 match {
    |    case (i @ _) if i.>(5) => i
    |  }))(scala.collection.immutable.List.canBuildFrom)
    |}""")
    *
    */

  @Test def testObjectWithPatternMatch3(): Unit = assertResultCode(
    code = sm"""
    |object PM3 {
    |  List(1, 2).map {
    |    case i: Int => i
    |  }
    |}""")(
    parsedCode = sm"""
    |object PM3 {
    |  List(1, 2).map({
    |    case (i @ ((_): Int)) => i
    |  })
    |}""")
    /*
    typedCode = sm"""
    |object PM3 {
    |  scala.collection.immutable.List.apply(1, 2).map(((x0$$2) => x0$$2 match {
    |    case (i @ ((_): scala.Int)) => i
    |  }))(scala.collection.immutable.List.canBuildFrom)
    |}""")
    *
    */

  @Test def testObjectWithPatternMatch4(): Unit = assertResultCode(
    code = sm"""
    |object PM4 {
    |  List(1, 2).map {
    |    case _ => 42
    |  }
    |}""")(
    parsedCode = sm"""
    |object PM4 {
    |  List(1, 2).map({
    |    case _ => 42
    |  })
    |}""")
    /*
    typedCode = sm"""
    |object PM4 {
    |  scala.collection.immutable.List.apply(1, 2).map(((x0$$3) => x0$$3 match {
    |    case _ => 42
    |  }))(scala.collection.immutable.List.canBuildFrom)
    |}""")
    *
    */

  @Test def testObjectWithPatternMatch5(): Unit = assertResultCode(
    code = sm"""
    |object PM5 {
    |  List(1, 2) match {
    |    case x :: xs => x
    |  }
    |}""")(
    parsedCode = sm"""
    |object PM5 {
    |  List(1, 2) match {
    |    case ::((x @ _), (xs @ _)) => x
    |  }
    |}""",
    typedCode = sm"""
    |object PM5 {
    |  scala.`package`.List.apply[Int](1, 2) match {
    |    case scala.`package`.::((x @ _), (xs @ _)) => x
    |  }
    |}""")

  @Test def testObjectWithPatternMatch6(): Unit = assertResultCode(
    code = sm"""
    |object PM6 {
    |  List(1, 2).map {
    |    case (0 | 1) => true
    |    case _ => false
    |  }
    |}""")(
    parsedCode = sm"""
    |object PM6 {
    |  List(1, 2).map({
    |    case (0| 1) => true
    |    case _ => false
    |  })
    |}""")
    /*
    typedCode = sm"""
    |object PM6 {
    |  scala.collection.immutable.List.apply(1, 2).map(((x0$$4) => x0$$4 match {
    |    case (0| 1) => true
    |    case _ => false
    |  }))(scala.collection.immutable.List.canBuildFrom)
    |}"""
    *
    */

  @Test def testObjectWithPatternMatch7(): Unit = assertPrintedCode(sm"""
    |object PM7 {
    |  scala.Predef.wrapString("abcde").toList match {
    |    case scala.collection.Seq((car @ _), _*) => car
    |  }
    |}""")

  @Test def testObjectWithPatternMatch8(): Unit = assertPrintedCode(sm"""
    |{
    |  object Extractor {
    |    def unapply(i: scala.Int) = scala.Some.apply[scala.Int](i)
    |  };
    |  object PM9 {
    |    42 match {
    |      case (a @ Extractor((i @ _))) => i
    |    }
    |  };
    |  ()
    |}""")

  @Test def testObjectWithPartialFunc(): Unit = assertPrintedCode(sm"""
    |object Test {
    |  def partFuncTest[A, B](e: scala.`package`.Either[A, B]): scala.Unit = e match {
    |    case scala.`package`.Right(_) => ()
    |  }
    |}""")

  @Test def testObjectWithTry(): Unit = assertResultCode(
    code = sm"""
    |object Test {
    |  import java.io._;
    |  var file: PrintStream = null;
    |  try {
    |    val out = new FileOutputStream("myfile.txt");
    |    file = new PrintStream(out)
    |  } catch {
    |    case ioe: IOException => println("ioe")
    |    case e: Exception => println("e")
    |  } finally println("finally")
    |}""")(
    parsedCode = sm"""
    |object Test {
    |  import java.io._;
    |  var file: PrintStream = null;
    |  try {
    |    val out = new FileOutputStream("myfile.txt");
    |    file = new PrintStream(out)
    |  } catch {
    |    case (ioe @ ((_): IOException)) => println("ioe")
    |    case (e @ ((_): Exception)) => println("e")
    |  } finally println("finally")
    |}""",
    typedCode = sm"""
    |object Test {
    |  import java.io._;
    |  var file: java.io.PrintStream = null;
    |  try {
    |    val out = new java.io.FileOutputStream("myfile.txt");
    |    Test.this.`file_=`(new java.io.PrintStream(out))
    |  } catch {
    |    case (ioe @ ((_): java.io.IOException)) => scala.Predef.println("ioe")
    |    case (e @ ((_): scala.`package`.Exception)) => scala.Predef.println("e")
    |  } finally scala.Predef.println("finally")
    |}""")
}

@RunWith(classOf[JUnit4])
class TraitPrintTest {
  @Test def testTrait(): Unit = assertPrintedCode("trait *")

  @Test def testTraitWithBody(): Unit = assertPrintedCode(sm"""
    |trait X {
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelfTypeAndBody(): Unit = assertPrintedCode(sm"""
    |trait X { self: scala.`package`.Cloneable =>
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelf1(): Unit = assertPrintedCode(sm"""
    |trait X { self =>
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelf2(): Unit = assertPrintedCode(sm"""
    |trait X { self: scala.`package`.Cloneable with scala.`package`.Serializable =>
    |  val x: scala.Int = 1
    |}""")

  @Test def testTraitTypeParams(): Unit = assertPrintedCode("trait X[A, B]")

  @Test def testTraitWithBody2(): Unit = assertPrintedCode(sm"""
    |trait X {
    |  def foo: scala.Unit;
    |  val bar: scala.Predef.String
    |}""")

  @Test def testTraitWithInh(): Unit = assertPrintedCode("trait X extends scala.`package`.Cloneable with scala.`package`.Serializable")

  @Test def testTraitWithEarly1(): Unit = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: Int = 1
    |} with AnyRef""", checkTypedTree = false)

  @Test def testTraitWithEarly2(): Unit = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: scala.Int = 0;
    |  type Foo = scala.Unit
    |} with scala.`package`.Cloneable""")

  @Test def testTraitWithEarly3(): Unit = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: scala.Int = 5;
    |  val y: scala.Double = 4.0;
    |  type Foo;
    |  type XString = scala.Predef.String
    |} with scala.`package`.Serializable""")

  @Test def testTraitWithEarly4(): Unit = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: scala.Int = 5;
    |  val y: scala.Double = 4.0;
    |  type Foo;
    |  type XString = scala.Predef.String
    |} with scala.`package`.Serializable {
    |  val z: scala.Int = 7
    |}""")

  @Test def testTraitWithSingletonTypeTree(): Unit = assertPrintedCode(sm"""
    |trait Test {
    |  def testReturnSingleton(): Test.this.type
    |}""")

  @Test def testTraitWithThis(): Unit = assertTreeCode(q"trait Test { this: X with Y => }")(sm"""
    |trait Test { _ : X with Y =>
    |  
    |}""")

  @Test def testTraitWithWhile1(): Unit = assertPrintedCode(sm"""
    |trait Test {
    |  while (false) 
    |    scala.Predef.println("testing...")
    |  
    |}""")

  @Test def testTraitWithWhile2(): Unit = assertPrintedCode(sm"""
    |trait Test {
    |  while (true) 
    |    {
    |      scala.Predef.println("testing...");
    |      scala.Predef.println("testing...")
    |    }
    |  
    |}""")

  @Test def testTraitWithDoWhile1(): Unit = assertPrintedCode(sm"""
    |trait Test {
    |  do 
    |    scala.Predef.println("testing...")
    |   while (true) 
    |}""")  

  @Test def testTraitWithTypes(): Unit = assertResultCode(
    code = sm"""
    |trait Test {
    |  type A = Int;
    |  type B >: Nothing <: AnyRef;
    |  protected type C >: Nothing;
    |  type D <: AnyRef
    |}""")(
    parsedCode = sm"""
    |trait Test {
    |  type A = Int;
    |  type B >: Nothing <: AnyRef;
    |  protected type C >: Nothing;
    |  type D <: AnyRef
    |}""",
    typedCode = sm"""
    |trait Test {
    |  type A = scala.Int;
    |  type B <: scala.AnyRef;
    |  protected type C;
    |  type D <: scala.AnyRef
    |}""")
}

@RunWith(classOf[JUnit4])
class ValAndDefPrintTest {
  @Test def testVal1(): Unit = assertPrintedCode("val a: scala.Unit = ()")

  @Test def testVal2(): Unit = assertPrintedCode("val * : scala.Unit = ()")

  @Test def testVal3(): Unit = assertPrintedCode("val a_ : scala.Unit = ()")

  @Test def testDef1(): Unit = assertPrintedCode("def a = ()")

  @Test def testDef2(): Unit = assertPrintedCode("def * : scala.Unit = ()")

  @Test def testDef3(): Unit = assertPrintedCode("def a_(x: scala.Int): scala.Unit = ()")

  @Test def testDef4(): Unit = assertPrintedCode("def a_ : scala.Unit = ()")

  @Test def testDef5(): Unit = assertPrintedCode("def a_(* : scala.Int): scala.Unit = ()")

  @Test def testDef6(): Unit = assertPrintedCode("def a_(b_ : scala.Int) = ()")

  @deprecated("Tests deprecated API", since="2.13")
  @Test def testDef7(): Unit = assertTreeCode{
    Block(
      DefDef(NoMods, TermName("test1"), Nil, Nil, EmptyTree, Literal(Constant(()))),
      DefDef(NoMods, TermName("test2"), Nil, Nil :: Nil, EmptyTree, Literal(Constant(())))
    )
  }(sm"""
    |{
    |  def test1 = ();
    |  def test2() = ()
    |}""")

  @Test def testDef8(): Unit = {
    val arg = ValDef(Modifiers(Flag.IMPLICIT) , TermName("a"),
      AppliedTypeTree(Ident(TypeName("R")), List(Ident(TypeName("X")))), EmptyTree)

    //def m[X](implicit a: R[X]) = ()
    val tree = DefDef(NoMods, TermName("test"), TypeDef(NoMods, TypeName("X"), Nil, EmptyTree) :: Nil,
      List(List(arg)), EmptyTree, Literal(Constant(())))

    assertTreeCode(tree)("def test[X](implicit a: R[X]) = ()")
  }

  @Test def testDef9(): Unit = assertPrintedCode("def a(x: scala.Int)(implicit z: scala.Double, y: scala.Float): scala.Unit = ()")

  @Test def testDefWithLazyVal1(): Unit = assertPrintedCode(sm"""
    |def a = {
    |  lazy val test: scala.Int = 42;
    |  ()
    |}
    """)

  @Test def testDefWithLazyVal2(): Unit = assertPrintedCode(sm"""
    |def a = {
    |  lazy val test: scala.Unit = {
    |    scala.Predef.println();
    |    scala.Predef.println()
    |  };
    |  ()
    |}""")

  @Test def testDefWithParams1(): Unit = assertPrintedCode("def foo(x: scala.Int*) = ()")

  @Test def testDefWithParams2(): Unit = assertPrintedCode(sm"""
    |{
    |  def foo(x: scala.Int)(y: scala.Int = 1) = ();
    |  ()
    |}""")

  @Test def testDefWithTypeParams1(): Unit = assertPrintedCode(sm"""
    |{
    |  def foo[A, B, C](x: A)(y: scala.Int = 1): C = ().asInstanceOf[C];
    |  ()
    |}""")

  @Test def testDefWithTypeParams2(): Unit = assertPrintedCode("def foo[A, B <: scala.AnyVal] = ()")

  @Test def testDefWithAnn1(): Unit = assertPrintedCode("@annot def foo = null", checkTypedTree = false)

  @Test def testDefWithAnn2(): Unit = assertPrintedCode("@a(x) def foo = null", checkTypedTree = false)

  @Test def testDefWithAnn3(): Unit = assertPrintedCode("@Foo[A, B] def foo = null", checkTypedTree = false)

  @Test def testDefWithAnn4(): Unit = assertPrintedCode("@Foo(a)(b)(x, y) def foo = null", checkTypedTree = false)

  @Test def testDefWithAnn5(): Unit = assertPrintedCode("@Foo[A, B](a)(b) @Bar def foo(x: Int) = null", checkTypedTree = false)

  @Test def testDefWithAnn6(): Unit = assertPrintedCode("@test1(new test2()) def foo = 42", checkTypedTree = false)

  @Test def testDefWithAnn7(): Unit = assertPrintedCode("@`t*` def foo = 42", checkTypedTree = false)

  @Test def testDefWithAnn8(): Unit = assertPrintedCode("@throws(classOf[Exception]) def foo = throw new Exception()", checkTypedTree = false)

  @Test def testAnnotated1(): Unit = assertResultCode(
    code = "def foo = 42: @baz")(
    parsedCode = "def foo = 42: @baz",
    typedCode = "def foo = (42: @baz)",
    wrap = true)

  @Test def testAnnotated2(): Unit = assertResultCode(
    code = "def foo = 42: @foo2[A1, B1](4)(2)")(
    parsedCode = "def foo = 42: @foo2[A1, B1](4)(2)",
    typedCode = "def foo = (42: @foo2[A1, B1](4)(2))",
    wrap = true)

  @Test def testAnnotated3(): Unit = assertResultCode(
    code = "def foo = (42: @foo1[A1, B1]): @foo2[A1, B1](4)(2)")(
    parsedCode = "def foo = (42: @foo1[A1, B1]): @foo2[A1, B1](4)(2)",
    typedCode = "def foo = ((42: @foo1[A1, B1]): @foo2[A1, B1](4)(2))",
    wrap = true)

  @Test def testAnnotated4(): Unit = assertResultCode(
    code = "def foo = 42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]())")(
    parsedCode = "def foo = 42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]())",
    typedCode = "def foo = (42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]()))",
    wrap = true)

  @Test def testAnnotated5(): Unit = assertPrintedCode(sm"""
    |{
    |  val x = 5;
    |  (x: @unchecked) match {
    |    case ((_): scala.Int) => true
    |    case _ => false
    |  }
    |}""")

  @Test def testAnnotated8(): Unit = assertPrintedCode(sm"""
    |{
    |  val x = 5;
    |  ((x: @unchecked): @foo3(4)(2.0F, new foo1[A1, B1]())) match {
    |    case _ => true
    |  }
    |}""", wrapCode = true)
}

@RunWith(classOf[JUnit4])
class PackagePrintTest {
  @Test def testPackage1(): Unit = assertPrintedCode(sm"""
    |package foo.bar {
    |  
    |}""", checkTypedTree = false)

  @Test def testPackage2(): Unit = assertPrintedCode(sm"""
    |package foo {
    |  class C
    |
    |  object D
    |}""", checkTypedTree = false)

  //package object foo extends a with b
  @Test def testPackage3(): Unit = assertPrintedCode(sm"""
    |package foo {
    |  object `package` extends a with b
    |}""", checkTypedTree = false)

  //package object foo { def foo; val x = 1 }
  @Test def testPackage4(): Unit = assertPrintedCode(sm"""
    |package foo {
    |  object `package` {
    |    def foo: scala.Unit = ();
    |    val x = 1
    |  }
    |}""", checkTypedTree = false)

  //package object foo extends { val x = 1; type I = Int } with Any
  @Test def testPackage5(): Unit = assertPrintedCode(sm"""
    |package foo {
    |  object `package` extends {
    |    val x = 1;
    |    type I = Int
    |  } with AnyRef
    |}""", checkTypedTree = false)
}

@RunWith(classOf[JUnit4])
class QuasiTreesPrintTest {
  @Test def testQuasiIdent(): Unit = assertTreeCode(q"*")("*")

  @Test def testQuasiVal(): Unit = assertTreeCode(q"val * : Unit = null")("val * : Unit = null")

  @Test def testQuasiDef(): Unit = assertTreeCode(q"def * : Unit = null")("def * : Unit = null")

  @Test def testQuasiTrait(): Unit = assertTreeCode(q"trait *")("trait *")

  @Test def testQuasiClass(): Unit = assertTreeCode(q"class *")("class *")

  @Test def testQuasiClassWithPublicParams(): Unit = assertTreeCode(q"class X(val x: Int, val s:String)")("class X(val x: Int, val s: String)")

  @Test def testQuasiClassWithParams(): Unit = assertTreeCode(q"class X(x: Int, s:String)")("class X(x: Int, s: String)")

  @Test def testQuasiObject(): Unit = assertTreeCode(q"object *")("object *")

  @Test def testQuasiObjectWithBody(): Unit = assertTreeCode(q"""object X{ def y = "test" }""")(sm"""
    |object X {
    |  def y = "test"
    |}""")

  @Test def testQuasiClassWithBody(): Unit = assertTreeCode(q"""class X{ def y = "test" }""")(sm"""
    |class X {
    |  def y = "test"
    |}""")

  @Test def testQuasiTraitWithBody(): Unit = assertTreeCode(q"""trait X{ def y = "test" }""")(sm"""
    |trait X {
    |  def y = "test"
    |}""")

  @Test def testQuasiTraitWithSelfTypeAndBody(): Unit = assertTreeCode(q"""trait X{ self: Order => def y = "test" }""")(sm"""
    |trait X { self: Order =>
    |  def y = "test"
    |}""")

  @Test def testQuasiTraitWithSelf(): Unit = assertTreeCode(q"""trait X{ self => def y = "test" }""")(sm"""
    |trait X { self =>
    |  def y = "test"
    |}""")

  @Test def testQuasiCaseClassWithBody(): Unit = assertTreeCode(q"""case class X() { def y = "test" }""")(sm"""
    |case class X() {
    |  def y = "test"
    |}""")

  @Test def testQuasiCaseClassWithParamsAndBody(): Unit = assertTreeCode(q"""case class X(x: Int, s: String){ def y = "test" }""")(sm"""
    |case class X(x: Int, s: String) {
    |  def y = "test"
    |}""")

  @Test def testQuasiCaseClassWithTypes1(): Unit = assertTreeCode(q"""case class X(x: ${typeOf[Int]}, s: ${typeOf[String]}){ def y = "test" }""")(sm"""
    |case class X(x: Int, s: String) {
    |  def y = "test"
    |}""")

  @Test def testQuasiCaseClassWithTypes2(): Unit = assertTreeCode(q"""case class X(x: ${typeOf[Int]}, s: ${typeOf[String]}){ def y = "test" }""", typecheck = true)(sm"""
    |{
    |  case class X(x: Int, s: String) {
    |    def y = "test"
    |  };
    |  ()
    |}""")
}
