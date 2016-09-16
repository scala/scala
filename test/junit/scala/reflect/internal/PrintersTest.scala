package scala.reflect.internal

import org.junit.Test
import org.junit.Assert._
import scala.tools.reflect._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class PrintersTest extends BasePrintTests
  with ClassPrintTests
  with TraitPrintTests
  with ValAndDefPrintTests
  with QuasiTreesPrintTests
  with PackagePrintTests

object PrinterHelper {
  val toolbox = cm.mkToolBox()

  import scala.reflect.internal.Chars._
  private def normalizeEOL(resultCode: String) =
    resultCode.lines mkString s"$LF"

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
      |${source.trim.lines map {"  " + _} mkString s"$LF"}
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

trait BasePrintTests {
  @Test def testIdent = assertTreeCode(Ident("*"))("*")

  @Test def testConstant1 = assertTreeCode(Literal(Constant("*")))("\"*\"")

  @Test def testConstant2 = assertTreeCode(Literal(Constant(42)))("42")

  @Test def testConstantFloat = assertTreeCode(Literal(Constant(42f)))("42.0F")

  @Test def testConstantDouble = assertTreeCode(Literal(Constant(42d)))("42.0")

  @Test def testConstantLong = assertTreeCode(Literal(Constant(42l)))("42L")

  @Test def testConstantMultiline = assertTreeCode(Literal(Constant("hello\nworld")))("\"\"\"hello\nworld\"\"\"")

  val sq  = "\""
  val teq = "\\\"" * 3
  val tq  = "\"" * 3

  @Test def testConstantEmbeddedTriple = assertTreeCode(Literal(Constant(s"${tq}hello${tq}\nworld")))(s"${sq}${teq}hello${teq}\\nworld${sq}")

  @Test def testOpExpr = assertPrintedCode("(5).+(4)", checkTypedTree = false)

  @Test def testName1 = assertPrintedCode("class test")

  @Test def testName2 = assertPrintedCode("class *")

  @Test def testName4 = assertPrintedCode("class `a*`")

  @Test def testName5 = assertPrintedCode("val :::: = 1")

  @Test def testName6 = assertPrintedCode("val `::::t` = 1")

  @Test def testName7 = assertPrintedCode("""class \/""")

  @Test def testName8 = assertPrintedCode("""class \\\\""")

  @Test def testName9 = assertPrintedCode("""class test_\/""")

  @Test def testName10 = assertPrintedCode("""class `*_*`""")

  @Test def testName11 = assertPrintedCode("""class `a_*`""")

  @Test def testName12 = assertPrintedCode("""class `*_a`""")

  @Test def testName13 = assertPrintedCode("""class a_a""")

  @Test def testName14 = assertPrintedCode("val x$11 = 5")

  @Test def testName15 = assertPrintedCode("class `[]`")

  @Test def testName16 = assertPrintedCode("class `()`")

  @Test def testName17 = assertPrintedCode("class `{}`")

  @Test def testName18 = assertPrintedCode("class <>")

  @Test def testName19 = assertPrintedCode("""class `class`""")

  @Test def testName20 = assertPrintedCode("""class `test name`""")
  
  @Test def testName21 = assertPrintedCode("""class `test.name`""")

  @Test def testIfExpr1 = assertResultCode(code = sm"""
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

  @Test def testIfExpr2 = assertPrintedCode(sm"""
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

  @Test def testIfExpr3 = assertPrintedCode(sm"""
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
  @Test def testBooleanExpr1 = assertPrintedCode("val x = true.&&(true).&&(false.`unary_!`)", checkTypedTree = false)

  //val x = true && !(true && false)
  @Test def testBooleanExpr2 = assertPrintedCode("val x = true.&&(true.&&(false).`unary_!`)", checkTypedTree = false)

  @Test def testNewExpr1 = assertResultCode(
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

  @Test def testNewExpr2 = assertResultCode(
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

  @Test def testNewExpr3 = assertPrintedCode(sm"""
    |{
    |  class foo[t];
    |  new foo[scala.Int]()
    |}""")

  @Test def testNewExpr4 = assertPrintedCode(sm"""
    |{
    |  class foo(x: scala.Int);
    |  val x = 5;
    |  new foo(x)
    |}""")

  @Test def testNewExpr5 = assertPrintedCode(sm"""
    |{
    |  class foo[t](x: scala.Int);
    |  val x = 5;
    |  new foo[scala.Predef.String](x)
    |}""")

  //new foo[t](x) { () }
  @Test def testNewExpr6 = assertResultCode(
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
  @Test def testNewExpr7 = assertPrintedCode(sm"""
    |{
    |  trait foo;
    |  trait bar;
    |  {
    |    final class $$anon extends foo with bar;
    |    new $$anon()
    |  }
    |}""")

  //new { anonymous }
  @Test def testNewExpr8 = assertPrintedCode(sm"""
    |{
    |  final class $$anon {
    |    5
    |  };
    |  new $$anon()
    |}""")

  //new { val early = 1 } with Parent[Int] { body }
  @Test def testNewExpr9 = assertPrintedCode(sm"""
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
  @Test def testNewExpr10 = assertPrintedCode(sm"""
    |{
    |  class Foo;
    |  {
    |    final class $$anon extends Foo { self =>
    |      
    |    };
    |    new $$anon()
    |  }
    |}""")

  @Test def testReturn = assertPrintedCode("def test: scala.Int = return 42")

  @Test def testFunc1 = assertResultCode(
    code = "List(1, 2, 3).map((i: Int) => i - 1)")(
    parsedCode = "List(1, 2, 3).map(((i: Int) => i.-(1)))",
    typedCode = sm"scala.collection.immutable.List.apply[Int](1, 2, 3).map[Int, List[Int]](((i: scala.Int) => i.-(1)))(scala.collection.immutable.List.canBuildFrom[Int])")

  @Test def testFunc2 = assertResultCode(
    code = "val sum: Seq[Int] => Int = _ reduceLeft (_+_)")(
    parsedCode = "val sum: _root_.scala.Function1[Seq[Int], Int] = ((x$1) => x$1.reduceLeft(((x$2, x$3) => x$2.+(x$3))))",
    typedCode = "val sum: _root_.scala.Function1[scala.`package`.Seq[scala.Int], scala.Int] = ((x$1: Seq[Int]) => x$1.reduceLeft[Int](((x$2: Int, x$3: Int) => x$2.+(x$3))))")

  @Test def testFunc3 = assertResultCode(
    code = "List(1, 2, 3) map (_ - 1)")(
    parsedCode = "List(1, 2, 3).map(((x$1) => x$1.-(1))) ",
    typedCode = "scala.collection.immutable.List.apply[Int](1, 2, 3).map[Int, List[Int]](((x$1: Int) => x$1.-(1)))(scala.collection.immutable.List.canBuildFrom[Int])")

  @Test def testFunc4 = assertResultCode(
    code = "val x: String => Int = ((str: String) => 1)")(
    parsedCode = "val x: _root_.scala.Function1[String, Int] = ((str: String) => 1)",
    typedCode = " val x: _root_.scala.Function1[_root_.scala.Predef.String, _root_.scala.Int] = ((str: _root_.scala.Predef.String) => 1)", printRoot = true)

  @Test def testAssign1 = assertPrintedCode("(f.v = 5).toString", checkTypedTree = false)
  
  @Test def testAssign2 = assertPrintedCode("(f.v = 5)(2)", checkTypedTree = false)

  @Test def testImport1 = assertPrintedCode("import scala.collection.mutable")

  @Test def testImport2 = assertPrintedCode("import java.lang.{String=>Str}")

  @Test def testImport3 = assertPrintedCode("import java.lang.{String=>Str, Object=>_, _}")

  @Test def testImport4 = assertPrintedCode("import scala.collection._")
}

trait ClassPrintTests {
  @Test def testClass = assertPrintedCode("class *")

  @Test def testClassWithBody = assertPrintedCode(sm"""
    |class X {
    |  def y = "test"
    |}""")

  @Test def testClassConstructorModifiers = assertPrintedCode("class X private (x: scala.Int)")

  @Test def testClassConstructorModifierVisibility = assertPrintedCode(sm"""
    |object A {
    |  class X protected[A] (x: scala.Int)
    |}""")

  @Test def testClassWithPublicParams = assertPrintedCode("class X(val x: scala.Int, val s: scala.Predef.String)")

  @Test def testClassWithParams1 = assertPrintedCode("class X(x: scala.Int, s: scala.Predef.String)")

  @Test def testClassWithParams2 = assertPrintedCode("class X(@test x: Int, s: String)", checkTypedTree = false)

  @Test def testClassWithParams3 = assertPrintedCode("class X(implicit x: Int, s: String)", checkTypedTree = false)

  @Test def testClassWithParams4 = assertPrintedCode("class X(implicit @unchecked x: Int, s: String)", checkTypedTree = false)

  @Test def testClassWithParams5 = assertPrintedCode(sm"""
    |{
    |  class Y {
    |    val x = 5
    |  };
    |  class X(override private[this] val x: scala.Int, s: scala.Predef.String) extends Y;
    |  ()
    |}""")

  @Test def testClassWithParams6 = assertPrintedCode("class X(@test1 override private[this] val x: Int, @test2(param1 = 7) s: String) extends Y", checkTypedTree = false)

  @Test def testClassWithParams7 = assertPrintedCode("class X protected (val x: scala.Int, val s: scala.Predef.String)")

  @Test def testClassWithParams8 = assertPrintedCode("class X(var x: scala.Int)")

  @Test def testClassWithParams9 = assertPrintedCode("def test(x: scala.Int*) = 5")

  @Test def testClassWithByNameParam = assertPrintedCode("class X(x: => scala.Int)")

  @Test def testClassWithDefault = assertPrintedCode(sm"""
    |{
    |  class X(var x: scala.Int = 5);
    |  ()
    |}""")

  @Test def testClassWithParams10 = assertPrintedCode("class X(protected[zzz] var x: Int)", checkTypedTree = false)

  @Test def testClassWithParams11 = assertPrintedCode(sm"""
    |{
    |  class F(x: scala.Int);
    |  trait E {
    |    var x: scala.Int
    |  };
    |  class X(override var x: scala.Int = 5) extends F(x) with E;
    |  ()
    |}""")

  @Test def testClassWithParams12 = assertPrintedCode("class X(val y: scala.Int)()(var z: scala.Double)")

  @Test def testClassWithImplicitParams = assertPrintedCode("class X(var i: scala.Int)(implicit val d: scala.Double, var f: scala.Float)")

  @Test def testClassWithEarly =
    assertPrintedCode(sm"""
    |class X(var i: scala.Int) extends {
    |  val a = i;
    |  type B
    |} with scala.Serializable""")

  @Test def testClassWithThrow1 = assertPrintedCode(sm"""
    |class Throw1 {
    |  throw new scala.`package`.Exception("exception!")
    |}""")  

  @Test def testClassWithThrow2 = assertPrintedCode(sm"""
    |class Throw2 {
    |  var msg = "   ";
    |  val e = new scala.`package`.Exception(Throw2.this.msg);
    |  throw Throw2.this.e
    |}""")

  @Test def testClassWithAssignmentWithTuple1 = assertResultCode(sm"""
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

  @Test def testClassWithAssignmentWithTuple2 = assertResultCode(
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
  @Test def testClassWithPatternMatchInAssignment = assertPrintedCode(sm"""
    |class Test {
    |  private[this] val x$$1 = (scala.collection.immutable.List.apply[scala.Int](1, 3, 5): @scala.unchecked) match {
    |    case scala.collection.immutable.List((one @ _), (three @ _), (five @ _)) => scala.Tuple3.apply[scala.Int, scala.Int, scala.Int](one, three, five)
    |  };
    |  val one = Test.this.x$$1._1;
    |  val three = Test.this.x$$1._2;
    |  val five = Test.this.x$$1._3
    |}""")

  //class A(l: List[_])
  @Test def testClassWithExistentialParameter1 = assertPrintedCode(sm"""
    |class Test(l: (scala.`package`.List[_$$1] forSome { 
    |  type _$$1
    |}))""")

  @Test def testClassWithExistentialParameter2 = assertPrintedCode(sm"""
    |class B(l: (scala.`package`.List[T] forSome { 
    |  type T
    |}))""")

  @Test def testClassWithCompoundTypeTree = assertPrintedCode(sm"""
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

  @Test def testClassWithSelectFromTypeTree = assertPrintedCode(sm"""
    |{
    |  trait A {
    |    type T
    |  };
    |  class B(t: (A)#T);
    |  ()
    |}""")

  @Test def testImplicitClass = assertPrintedCode(sm"""
    |{
    |  implicit class X(protected[this] var x: scala.Int);
    |  ()
    |}""",
    checkTypedTree = true)

  @Test def testAbstractClass = assertPrintedCode("abstract class X(protected[this] var x: scala.Int)")

  @Test def testCaseClassWithParams1 = assertPrintedCode(sm"""
    |{
    |  case class X(x: scala.Int, s: scala.Predef.String);
    |  ()
    |}""")

  @Test def testCaseClassWithParams2 = assertPrintedCode(sm"""
    |{
    |  case class X(protected val x: scala.Int, s: scala.Predef.String);
    |  ()
    |}""")

  @Test def testCaseClassWithParams3 = assertPrintedCode(sm"""
    |{
    |  case class X(implicit x: scala.Int, s: scala.Predef.String);
    |  ()
    |}""")

  @Test def testCaseClassWithParams4 = assertPrintedCode(sm"""
    |{
    |  trait V {
    |    val x: scala.Int
    |  };
    |  case class X(override val x: scala.Int, s: scala.Predef.String) extends scala.Cloneable;
    |  ()
    |}""")

  @Test def testCaseClassWithBody = assertPrintedCode(sm"""
    |{
    |  case class X() {
    |    def y = "test"
    |  };
    |  ()
    |}""")

  @Test def testLocalClass = assertPrintedCode(sm"""
    |def test = {
    |  class X(var a: scala.Int) {
    |    def y = "test"
    |  };
    |  new X(5)
    |}""")

  @Test def testLocalCaseClass = assertPrintedCode(sm"""
    |def test = {
    |  case class X(var a: scala.Int) {
    |    def y = "test"
    |  };
    |  new X(5)
    |}""")

  @Test def testSuperInClass = assertPrintedCode(sm"""
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

  @Test def testThisInClass = assertPrintedCode(sm"""
    |class Outer {
    |  class Inner {
    |    val outer = Outer.this
    |  };
    |  val self = this
    |}""")

  @Test def testCaseClassWithParamsAndBody = assertPrintedCode(sm"""
    |{
    |  case class X(var x: scala.Int, var s: scala.Predef.String) {
    |    def y = "test"
    |  };
    |  ()
    |}""")

  @Test def testObject = assertPrintedCode("object *")

  @Test def testObjectWithBody = assertPrintedCode(sm"""
    |object X {
    |  def y = "test"
    |}""")

  @Test def testObjectWithEarly1 = assertPrintedCode(sm"""
    |object X extends {
    |  val early: scala.Int = 42
    |} with scala.Serializable""")

  @Test def testObjectWithEarly2 = assertPrintedCode(sm"""
    |object X extends {
    |  val early: scala.Int = 42;
    |  type EarlyT = scala.Predef.String
    |} with scala.Serializable""")

  @Test def testObjectWithSelf = assertPrintedCode(sm"""
    |object Foo extends scala.Serializable { self =>
    |  42
    |}""")

  @Test def testObjectInh = assertPrintedCode(sm"""
    |trait Y {
    |  private[Y] object X extends scala.Serializable with scala.Cloneable
    |}""")

  @Test def testObjectWithPatternMatch1 = assertPrintedCode(sm"""
    |object PM1 {
    |  scala.collection.immutable.List.apply[scala.Int](1, 2) match {
    |    case (i @ _) => i
    |  }
    |}""")

  @Test def testObjectWithPatternMatch2 = assertResultCode(
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

  @Test def testObjectWithPatternMatch3 = assertResultCode(
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

  @Test def testObjectWithPatternMatch4 = assertResultCode(
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

  @Test def testObjectWithPatternMatch5 = assertResultCode(
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
    |  scala.collection.immutable.List.apply[Int](1, 2) match {
    |    case scala.`package`.::((x @ _), (xs @ _)) => x
    |  }
    |}""")

  @Test def testObjectWithPatternMatch6 = assertResultCode(
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

  @Test def testObjectWithPatternMatch7 = assertPrintedCode(sm"""
    |object PM7 {
    |  scala.Predef.augmentString("abcde").toList match {
    |    case scala.collection.Seq((car @ _), _*) => car
    |  }
    |}""")

  @Test def testObjectWithPatternMatch8 = assertPrintedCode(sm"""
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

  @Test def testObjectWithPartialFunc = assertPrintedCode(sm"""
    |object Test {
    |  def partFuncTest[A, B](e: scala.`package`.Either[A, B]): scala.Unit = e match {
    |    case scala.`package`.Right(_) => ()
    |  }
    |}""")

  @Test def testObjectWithTry = assertResultCode(
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

trait TraitPrintTests {
  @Test def testTrait = assertPrintedCode("trait *")

  @Test def testTraitWithBody = assertPrintedCode(sm"""
    |trait X {
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelfTypeAndBody = assertPrintedCode(sm"""
    |trait X { self: scala.Cloneable =>
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelf1 = assertPrintedCode(sm"""
    |trait X { self =>
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelf2 = assertPrintedCode(sm"""
    |trait X { self: scala.Cloneable with scala.Serializable =>
    |  val x: scala.Int = 1
    |}""")

  @Test def testTraitTypeParams = assertPrintedCode("trait X[A, B]")

  @Test def testTraitWithBody2 = assertPrintedCode(sm"""
    |trait X {
    |  def foo: scala.Unit;
    |  val bar: scala.Predef.String
    |}""")

  @Test def testTraitWithInh = assertPrintedCode("trait X extends scala.Cloneable with scala.Serializable")

  @Test def testTraitWithEarly1 = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: Int = 1
    |} with AnyRef""", checkTypedTree = false)

  @Test def testTraitWithEarly2 = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: scala.Int = 0;
    |  type Foo = scala.Unit
    |} with scala.Cloneable""")

  @Test def testTraitWithEarly3 = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: scala.Int = 5;
    |  val y: scala.Double = 4.0;
    |  type Foo;
    |  type XString = scala.Predef.String
    |} with scala.Serializable""")

  @Test def testTraitWithEarly4 = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: scala.Int = 5;
    |  val y: scala.Double = 4.0;
    |  type Foo;
    |  type XString = scala.Predef.String
    |} with scala.Serializable {
    |  val z = 7
    |}""")

  @Test def testTraitWithSingletonTypeTree = assertPrintedCode(sm"""
    |trait Test {
    |  def testReturnSingleton(): Test.this.type
    |}""")

  @Test def testTraitWithThis = assertTreeCode(q"trait Test { this: X with Y => }")(sm"""
    |trait Test { _ : X with Y =>
    |  
    |}""")

  @Test def testTraitWithWhile1 = assertPrintedCode(sm"""
    |trait Test {
    |  while (false) 
    |    scala.Predef.println("testing...")
    |  
    |}""")

  @Test def testTraitWithWhile2 = assertPrintedCode(sm"""
    |trait Test {
    |  while (true) 
    |    {
    |      scala.Predef.println("testing...");
    |      scala.Predef.println("testing...")
    |    }
    |  
    |}""")

  @Test def testTraitWithDoWhile1 = assertPrintedCode(sm"""
    |trait Test {
    |  do 
    |    scala.Predef.println("testing...")
    |   while (true) 
    |}""")  

  @Test def testTraitWithTypes = assertResultCode(
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

trait ValAndDefPrintTests {
  @Test def testVal1 = assertPrintedCode("val a: scala.Unit = ()")

  @Test def testVal2 = assertPrintedCode("val * : scala.Unit = ()")

  @Test def testVal3 = assertPrintedCode("val a_ : scala.Unit = ()")

  @Test def testDef1 = assertPrintedCode("def a = ()")

  @Test def testDef2 = assertPrintedCode("def * : scala.Unit = ()")

  @Test def testDef3 = assertPrintedCode("def a_(x: scala.Int): scala.Unit = ()")

  @Test def testDef4 = assertPrintedCode("def a_ : scala.Unit = ()")

  @Test def testDef5 = assertPrintedCode("def a_(* : scala.Int): scala.Unit = ()")

  @Test def testDef6 = assertPrintedCode("def a_(b_ : scala.Int) = ()")

  @Test def testDef7 = assertTreeCode{ 
    Block(
      DefDef(NoMods, newTermName("test1"), Nil, Nil, EmptyTree, Literal(Constant(()))),
      DefDef(NoMods, newTermName("test2"), Nil, Nil :: Nil, EmptyTree, Literal(Constant(())))
    )
  }(sm"""
    |{
    |  def test1 = ();
    |  def test2() = ()
    |}""")

  @Test def testDef8 = {
    val arg = ValDef(Modifiers(Flag.IMPLICIT) , newTermName("a"),
      AppliedTypeTree(Ident(newTypeName("R")), List(Ident(newTypeName("X")))), EmptyTree)

    //def m[X](implicit a: R[X]) = ()
    val tree = DefDef(NoMods, newTermName("test"), TypeDef(NoMods, newTypeName("X"), Nil, EmptyTree) :: Nil,
      List(List(arg)), EmptyTree, Literal(Constant(())))

    assertTreeCode(tree)("def test[X](implicit a: R[X]) = ()")
  }

  @Test def testDef9 = assertPrintedCode("def a(x: scala.Int)(implicit z: scala.Double, y: scala.Float): scala.Unit = ()")

  @Test def testDefWithLazyVal1 = assertResultCode(
    code = "def a = { lazy val test: Int = 42 }")(
    parsedCode = sm"""
    |def a = {
    |  lazy val test: Int = 42;
    |  ()
    |}
    """,
    typedCode = sm"""
    |def a = {
    |  lazy val test: scala.Int = 42;
    |  ()
    |}""")

  @Test def testDefWithLazyVal2 = assertPrintedCode(sm"""
    |def a = {
    |  lazy val test: Unit = {
    |    scala.Predef.println();
    |    scala.Predef.println()
    |  };
    |  ()
    |}""")

  @Test def testDefWithParams1 = assertPrintedCode("def foo(x: scala.Int*) = ()")

  @Test def testDefWithParams2 = assertPrintedCode(sm"""
    |{
    |  def foo(x: scala.Int)(y: scala.Int = 1) = ();
    |  ()
    |}""")

  @Test def testDefWithTypeParams1 = assertPrintedCode(sm"""
    |{
    |  def foo[A, B, C](x: A)(y: scala.Int = 1): C = ().asInstanceOf[C];
    |  ()
    |}""")

  @Test def testDefWithTypeParams2 = assertPrintedCode("def foo[A, B <: scala.AnyVal] = ()")

  @Test def testDefWithAnn1 = assertPrintedCode("@annot def foo = null", checkTypedTree = false)

  @Test def testDefWithAnn2 = assertPrintedCode("@a(x) def foo = null", checkTypedTree = false)

  @Test def testDefWithAnn3 = assertPrintedCode("@Foo[A, B] def foo = null", checkTypedTree = false)

  @Test def testDefWithAnn4 = assertPrintedCode("@Foo(a)(b)(x, y) def foo = null", checkTypedTree = false)

  @Test def testDefWithAnn5 = assertPrintedCode("@Foo[A, B](a)(b) @Bar def foo(x: Int) = null", checkTypedTree = false)

  @Test def testDefWithAnn6 = assertPrintedCode("@test1(new test2()) def foo = 42", checkTypedTree = false)

  @Test def testDefWithAnn7 = assertPrintedCode("@`t*` def foo = 42", checkTypedTree = false)

  @Test def testDefWithAnn8 = assertPrintedCode("@throws(classOf[Exception]) def foo = throw new Exception()", checkTypedTree = false)

  @Test def testAnnotated1 = assertResultCode(
    code = "def foo = 42: @baz")(
    parsedCode = "def foo = 42: @baz",
    typedCode = "def foo = (42: @baz)",
    wrap = true)

  @Test def testAnnotated2 = assertResultCode(
    code = "def foo = 42: @foo2[A1, B1](4)(2)")(
    parsedCode = "def foo = 42: @foo2[A1, B1](4)(2)",
    typedCode = "def foo = (42: @foo2[A1, B1](4)(2))",
    wrap = true)

  @Test def testAnnotated3 = assertResultCode(
    code = "def foo = (42: @foo1[A1, B1]): @foo2[A1, B1](4)(2)")(
    parsedCode = "def foo = (42: @foo1[A1, B1]): @foo2[A1, B1](4)(2)",
    typedCode = "def foo = ((42: @foo1[A1, B1]): @foo2[A1, B1](4)(2))",
    wrap = true)

  @Test def testAnnotated4 = assertResultCode(
    code = "def foo = 42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]())")(
    parsedCode = "def foo = 42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]())",
    typedCode = "def foo = (42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]()))",
    wrap = true)

  @Test def testAnnotated5 = assertPrintedCode(sm"""
    |{
    |  val x = 5;
    |  (x: @unchecked) match {
    |    case ((_): scala.Int) => true
    |    case _ => false
    |  }
    |}""")

  @Test def testAnnotated8 = assertPrintedCode(sm"""
    |{
    |  val x = 5;
    |  ((x: @unchecked): @foo3(4)(2.0F, new foo1[A1, B1]())) match {
    |    case _ => true
    |  }
    |}""", wrapCode = true)
}

trait PackagePrintTests {
  @Test def testPackage1 = assertPrintedCode(sm"""
    |package foo.bar {
    |  
    |}""", checkTypedTree = false)

  @Test def testPackage2 = assertPrintedCode(sm"""
    |package foo {
    |  class C
    |
    |  object D
    |}""", checkTypedTree = false)

  //package object foo extends a with b
  @Test def testPackage3 = assertPrintedCode(sm"""
    |package foo {
    |  object `package` extends a with b
    |}""", checkTypedTree = false)

  //package object foo { def foo; val x = 1 }
  @Test def testPackage4 = assertPrintedCode(sm"""
    |package foo {
    |  object `package` {
    |    def foo: scala.Unit = ();
    |    val x = 1
    |  }
    |}""", checkTypedTree = false)

  //package object foo extends { val x = 1; type I = Int } with Any
  @Test def testPackage5 = assertPrintedCode(sm"""
    |package foo {
    |  object `package` extends {
    |    val x = 1;
    |    type I = Int
    |  } with AnyRef
    |}""", checkTypedTree = false)
}

trait QuasiTreesPrintTests {
  @Test def testQuasiIdent = assertTreeCode(q"*")("*")

  @Test def testQuasiVal = assertTreeCode(q"val * : Unit = null")("val * : Unit = null")

  @Test def testQuasiDef = assertTreeCode(q"def * : Unit = null")("def * : Unit = null")

  @Test def testQuasiTrait = assertTreeCode(q"trait *")("trait *")

  @Test def testQuasiClass = assertTreeCode(q"class *")("class *")

  @Test def testQuasiClassWithPublicParams = assertTreeCode(q"class X(val x: Int, val s:String)")("class X(val x: Int, val s: String)")

  @Test def testQuasiClassWithParams = assertTreeCode(q"class X(x: Int, s:String)")("class X(x: Int, s: String)")

  @Test def testQuasiObject = assertTreeCode(q"object *")("object *")

  @Test def testQuasiObjectWithBody = assertTreeCode(q"""object X{ def y = "test" }""")(sm"""
    |object X {
    |  def y = "test"
    |}""")

  @Test def testQuasiClassWithBody = assertTreeCode(q"""class X{ def y = "test" }""")(sm"""
    |class X {
    |  def y = "test"
    |}""")

  @Test def testQuasiTraitWithBody = assertTreeCode(q"""trait X{ def y = "test" }""")(sm"""
    |trait X {
    |  def y = "test"
    |}""")

  @Test def testQuasiTraitWithSelfTypeAndBody = assertTreeCode(q"""trait X{ self: Order => def y = "test" }""")(sm"""
    |trait X { self: Order =>
    |  def y = "test"
    |}""")

  @Test def testQuasiTraitWithSelf = assertTreeCode(q"""trait X{ self => def y = "test" }""")(sm"""
    |trait X { self =>
    |  def y = "test"
    |}""")

  @Test def testQuasiCaseClassWithBody = assertTreeCode(q"""case class X() { def y = "test" }""")(sm"""
    |case class X() {
    |  def y = "test"
    |}""")

  @Test def testQuasiCaseClassWithParamsAndBody = assertTreeCode(q"""case class X(x: Int, s: String){ def y = "test" }""")(sm"""
    |case class X(x: Int, s: String) {
    |  def y = "test"
    |}""")

  @Test def testQuasiCaseClassWithTypes1 = assertTreeCode(q"""case class X(x: ${typeOf[Int]}, s: ${typeOf[String]}){ def y = "test" }""")(sm"""
    |case class X(x: Int, s: String) {
    |  def y = "test"
    |}""")

  @Test def testQuasiCaseClassWithTypes2 = assertTreeCode(q"""case class X(x: ${typeOf[Int]}, s: ${typeOf[String]}){ def y = "test" }""", typecheck = true)(sm"""
    |{
    |  case class X(x: Int, s: String) {
    |    def y = "test"
    |  };
    |  ()
    |}""")
}
