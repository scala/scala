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
  def assertPrintedCode(code: String, tree: Tree = EmptyTree) = {
    def processEOL(resultCode: String) = {
      import scala.reflect.internal.Chars._
      resultCode.replaceAll(s"$CR$LF", s"$LF").replace(CR, LF)
    }

    val toolboxTree = 
      try{
        toolbox.parse(code)
      } catch {
        case e:scala.tools.reflect.ToolBoxError => throw new Exception(e.getMessage + ": " + code)
      }
    if (tree ne EmptyTree) assertEquals("using quasiquote or given tree"+"\n", code.trim, processEOL(showCode(tree)))
    else assertEquals("using toolbox parser", code.trim, processEOL(showCode(toolboxTree)))
  }
  
  implicit class StrContextStripMarginOps(val stringContext: StringContext) extends util.StripMarginInterpolator
}

import PrinterHelper._

trait BasePrintTests {
  @Test def testIdent = assertPrintedCode("*", Ident("*"))
  
  @Test def testConstant1 = assertPrintedCode("\"*\"", Literal(Constant("*")))
  
  @Test def testConstant2 = assertPrintedCode("42", Literal(Constant(42)))
  
  @Test def testConstantFloat = assertPrintedCode("42.0F", Literal(Constant(42f)))
  
  @Test def testConstantDouble = assertPrintedCode("42.0", Literal(Constant(42d)))
  
  @Test def testConstantLong = assertPrintedCode("42L", Literal(Constant(42l)))
  
  @Test def testOpExpr = assertPrintedCode("(5).+(4)")
  
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
  
  @Test def testIfExpr1 = assertPrintedCode(sm"""
    |if (a)
    |  ((expr1): Int)
    |else
    |  ((expr2): Int)""")
  
  @Test def testIfExpr2 = assertPrintedCode(sm"""
    |(if (a)
    |  {
    |    expr1;
    |    ()
    |  }
    |else
    |  {
    |    expr2;
    |    ()
    |  }).toString""")
  
  @Test def testIfExpr3 = assertPrintedCode(sm"""
    |(if (a)
    |  {
    |    expr1;
    |    ()
    |  }
    |else
    |  {
    |    expr2;
    |    ()
    |  }).method1().method2()""")
    
  //val x = true && true && false.!
  @Test def testBooleanExpr1 = assertPrintedCode("val x = true.&&(true).&&(false.!)")
  
  //val x = true && !(true && false)
  @Test def testBooleanExpr2 = assertPrintedCode("val x = true.&&(true.&&(false).`unary_!`)")
  
  @Test def testNewExpr1 = assertPrintedCode("new foo()")
  
  //new foo { test }
  @Test def testNewExpr2 = assertPrintedCode(sm"""
    |{
    |  final class $$anon extends foo {
    |    test
    |  };
    |  new $$anon()
    |}""")
    
  @Test def testNewExpr3 = assertPrintedCode("new foo[t]()")
  
  @Test def testNewExpr4 = assertPrintedCode("new foo(x)")
    
  @Test def testNewExpr5 = assertPrintedCode("new foo[t](x)")
    
  //new foo[t](x) { () }
  @Test def testNewExpr6 = assertPrintedCode(sm"""
    |{
    |  final class $$anon extends foo[t](x) {
    |    ()
    |  };
    |  new $$anon()
    |}""")
    
  //new foo with bar
  @Test def testNewExpr7 = assertPrintedCode(sm"""
    |{
    |  final class $$anon extends foo with bar;
    |  new $$anon()
    |}""")
    
  //new { anonymous }
  @Test def testNewExpr8 = assertPrintedCode(sm"""
    |{
    |  final class $$anon {
    |    anonymous
    |  };
    |  new $$anon()
    |}""")
    
  //new { val early = 1 } with Parent[Int] { body }
  @Test def testNewExpr9 = assertPrintedCode(sm"""
    |{
    |  final class $$anon extends {
    |    val early = 1
    |  } with Parent[Int] {
    |    body
    |  };
    |  new $$anon()
    |}""")
    
  //new Foo { self => }
  @Test def testNewExpr10 = assertPrintedCode(sm"""
    |{
    |  final class $$anon extends Foo { self =>
    |    
    |  };
    |  new $$anon()
    |}""")
    
  @Test def testReturn = assertPrintedCode("def test: Int = return 42")
  
  @Test def testFunc1 = assertPrintedCode("List(1, 2, 3).map(((i: Int) => i.-(1)))")

  //val sum: Seq[Int] => Int = _ reduceLeft (_+_)
  @Test def testFunc2 = assertPrintedCode("val sum: _root_.scala.Function1[Seq[Int], Int] = ((x$1) => x$1.reduceLeft(((x$2, x$3) => x$2.+(x$3))))")
  
  //List(1, 2, 3) map (_ - 1)
  @Test def testFunc3 = assertPrintedCode("List(1, 2, 3).map(((x$1) => x$1.-(1)))")
  
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

  @Test def testClassWithPublicParams = assertPrintedCode("class X(val x: Int, val s: String)")
  
  @Test def testClassWithParams1 = assertPrintedCode("class X(x: Int, s: String)")
  
  @Test def testClassWithParams2 = assertPrintedCode("class X(@test x: Int, s: String)")
  
  @Test def testClassWithParams3 = assertPrintedCode("class X(implicit x: Int, s: String)")
  
  @Test def testClassWithParams4 = assertPrintedCode("class X(implicit @test x: Int, s: String)")
  
  @Test def testClassWithParams5 = assertPrintedCode("class X(override private[this] val x: Int, s: String) extends Y")
  
  @Test def testClassWithParams6 = assertPrintedCode("class X(@test1 override private[this] val x: Int, @test2(param1 = 7) s: String) extends Y")
  
  @Test def testClassWithParams7 = assertPrintedCode("class X protected (val x: Int, val s: String)")
  
  @Test def testClassWithParams8 = assertPrintedCode("class X(var x: Int)")
  
  @Test def testClassWithParams9 = assertPrintedCode("class X(var x: Int*)")
  
  @Test def testClassWithByNameParam = assertPrintedCode("class X(x: => Int)")
  
  @Test def testClassWithDefault = assertPrintedCode("class X(var x: Int = 5)")
  
  @Test def testClassWithParams10 = assertPrintedCode("class X(protected[zzz] var x: Int)")
  
  @Test def testClassWithParams11 = assertPrintedCode("class X(override var x: Int) extends F(x) with E(x)")
  
  @Test def testClassWithParams12 = assertPrintedCode("class X(val y: Int)()(var z: Double)")
  
  @Test def testClassWithImplicitParams = assertPrintedCode("class X(var i: Int)(implicit val d: Double, var f: Float)")
  
  @Test def testClassWithEarly = assertPrintedCode(sm"""
    |class X(var i: Int) extends {
    |  val a: String = i;
    |  type B
    |} with Y""")

  @Test def testClassWithThrow1 = assertPrintedCode(sm"""
    |class Throw1 {
    |  throw new Exception("exception!")
    |}""")  
    
  @Test def testClassWithThrow2 = assertPrintedCode(sm"""
    |class Throw2 {
    |  var msg = "   ";
    |  val e = new Exception(msg);
    |  throw e
    |}""")
    
  /*
    class Test {
      val (a, b) = (1, 2)
    }
  */
  @Test def testClassWithAssignmentWithTuple1 = assertPrintedCode(sm"""
    |class Test {
    |  private[this] val x$$1 = (scala.Tuple2(1, 2): @scala.unchecked) match {
    |    case scala.Tuple2((a @ _), (b @ _)) => scala.Tuple2(a, b)
    |  };
    |  val a = x$$1._1;
    |  val b = x$$1._2
    |}""")
    
  /*
    class Test {
      val (a, b) = (1).->(2)
    }
  */
  @Test def testClassWithAssignmentWithTuple2 = assertPrintedCode(sm"""
    |class Test {
    |  private[this] val x$$1 = ((1).->(2): @scala.unchecked) match {
    |    case scala.Tuple2((a @ _), (b @ _)) => scala.Tuple2(a, b)
    |  };
    |  val a = x$$1._1;
    |  val b = x$$1._2
    |}""")
    
  /*
    class Test {
      val List(one, three, five) = List(1,3,5)
    }
  */
  @Test def testClassWithPatternMatchInAssignment = assertPrintedCode(sm"""
    |class Test {
    |  private[this] val x$$1 = (List(1, 3, 5): @scala.unchecked) match {
    |    case List((one @ _), (three @ _), (five @ _)) => scala.Tuple3(one, three, five)
    |  };
    |  val one = x$$1._1;
    |  val three = x$$1._2;
    |  val five = x$$1._3
    |}""")

  //class A(l: List[_])
  @Test def testClassWithExistentialParameter1 = assertPrintedCode(sm"""
    |class Test(l: (List[_$$1] forSome { 
    |  type _$$1
    |}))""")
    
  @Test def testClassWithExistentialParameter2 = assertPrintedCode(sm"""
    |class B(l: (List[T] forSome { 
    |  type T
    |}))""")
    
  @Test def testClassWithCompoundTypeTree = assertPrintedCode(sm"""
    |{
    |  trait A;
    |  trait B;
    |  abstract class C(val a: A with B) {
    |    def method(x: A with B with C {
    |      val x: Float
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
    
  @Test def testImplicitClass = assertPrintedCode("implicit class X(protected[zzz] var x: Int)")
    
  @Test def testAbstractClass = assertPrintedCode("abstract class X(protected[zzz] var x: Int)")
  
  @Test def testCaseClassWithParams1 = assertPrintedCode("case class X(x: Int, s: String)")
  
  @Test def testCaseClassWithParams2 = assertPrintedCode("case class X(protected val x: Int, s: String)")
  
  @Test def testCaseClassWithParams3 = assertPrintedCode("case class X(implicit x: Int, s: String)")
  
  @Test def testCaseClassWithParams4 = assertPrintedCode("case class X(override val x: Int, s: String) extends Y")
  
  @Test def testCaseClassWithBody = assertPrintedCode(sm"""
    |case class X() {
    |  def y = "test"
    |}""")

  @Test def testLocalClass = assertPrintedCode(sm"""
    |def test = {
    |  class X(var a: Int) {
    |    def y = "test"
    |  };
    |  new X(5)
    |}""")

  @Test def testLocalCaseClass = assertPrintedCode(sm"""
    |def test = {
    |  case class X(var a: Int) {
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
    |    val outer = Root.this
    |  };
    |  val self = this
    |}""")
    
  @Test def testCaseClassWithParamsAndBody = assertPrintedCode(sm"""
    |case class X(x: Int, s: String) {
    |  def y = "test"
    |}""")

  @Test def testObject = assertPrintedCode("object *")
  
  @Test def testObjectWithBody = assertPrintedCode(sm"""
    |object X {
    |  def y = "test"
    |}""")

  @Test def testObjectWithEarly1 = assertPrintedCode(sm"""
    |object X extends {
    |  val early: T = v
    |} with Bar""")
    
  @Test def testObjectWithEarly2 = assertPrintedCode(sm"""
    |object X extends {
    |  val early: T = v;
    |  type EarlyT = String
    |} with Bar""")

  @Test def testObjectWithSelf = assertPrintedCode(sm"""
    |object Foo extends Foo { self =>
    |  body
    |}""")

  @Test def testObjectInh = assertPrintedCode("private[Y] object X extends Bar with Baz")
  
  @Test def testObjectWithPatternMatch1 = assertPrintedCode(sm"""
    |object PM1 {
    |  List(1, 2) match {
    |    case (i @ _) => i
    |  }
    |}""")
  
  @Test def testObjectWithPatternMatch2 = assertPrintedCode(sm"""
    |object PM2 {
    |  List(1, 2).map({
    |    case (i @ _) if i.>(5) => i
    |  })
    |}""")
    
  //case i: Int => i  
  @Test def testObjectWithPatternMatch3 = assertPrintedCode(sm"""
    |object PM3 {
    |  List(1, 2).map({
    |    case (i @ ((_): Int)) => i
    |  })
    |}""")
    
  //case a @ (i: Int) => i
  @Test def testObjectWithPatternMatch4 = assertPrintedCode(sm"""
    |object PM4 {
    |  List(1, 2).map({
    |    case (a @ (i @ ((_): Int))) => i
    |  })
    |}""")
    
  @Test def testObjectWithPatternMatch5 = assertPrintedCode(sm"""
    |object PM5 {
    |  List(1, 2).map({
    |    case _ => 42
    |  })
    |}""")
    
  @Test def testObjectWithPatternMatch6 = assertPrintedCode(sm"""
    |object PM6 {
    |  List(1, 2) match {
    |    case ::((x @ _), (xs @ _)) => x
    |  }
    |}""")
    
  @Test def testObjectWithPatternMatch7 = assertPrintedCode(sm"""
    |object PM7 {
    |  List(1, 2).map({
    |    case (0| 1) => true
    |    case _ => false
    |  })
    |}""")
    
  @Test def testObjectWithPatternMatch8 = assertPrintedCode(sm"""
    |object PM8 {
    |  "abcde".toList match {
    |    case Seq((car @ _), _*) => car
    |  }
    |}""")
    
  @Test def testObjectWithPatternMatch9 = assertPrintedCode(sm"""
    |{
    |  object Extractor {
    |    def unapply(i: Int) = Some(i)
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
    |  def partFuncTest[A, B](e: Either[A, B]): scala.Unit = e match {
    |    case Right(_) => ()
    |  }
    |}""")

  @Test def testObjectWithTry = assertPrintedCode(sm"""
    |object Test {
    |  import java.io;
    |  var file: PrintStream = null;
    |  try {
    |    val out = new FileOutputStream("myfile.txt");
    |    file = new PrintStream(out)
    |  } catch {
    |    case (ioe @ ((_): IOException)) => println("ioe")
    |    case (e @ ((_): Exception)) => println("e")
    |  } finally println("finally")
    |}""")
}

trait TraitPrintTests {
  @Test def testTrait = assertPrintedCode("trait *")
  
  @Test def testTraitWithBody = assertPrintedCode(sm"""
    |trait X {
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelfTypeAndBody = assertPrintedCode(sm"""
    |trait X { self: Order =>
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelf1 = assertPrintedCode(sm"""
    |trait X { self =>
    |  def y = "test"
    |}""")
    
  @Test def testTraitWithSelf2 = assertPrintedCode(sm"""
    |trait X { self: Foo with Bar =>
    |  val x: Int = 1
    |}""")

  @Test def testTraitTypeParams = assertPrintedCode("trait X[A, B]")

  @Test def testTraitWithBody2 = assertPrintedCode(sm"""
    |trait X {
    |  def foo: scala.Unit;
    |  val bar: Baz
    |}""")
  
  @Test def testTraitWithInh = assertPrintedCode("trait X extends A with B")

  @Test def testTraitWithEarly1 = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: Int = 1
    |} with Any""")

  @Test def testTraitWithEarly2 = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: Int = 0;
    |  type Foo = Bar
    |} with Y""")
    
  @Test def testTraitWithEarly3 = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: Int = 5;
    |  val y: Double = 4.0;
    |  type Foo;
    |  type XString = String
    |} with Y""")
    
  @Test def testTraitWithEarly4 = assertPrintedCode(sm"""
    |trait X extends {
    |  val x: Int = 5;
    |  val y: Double = 4.0;
    |  type Foo;
    |  type XString = String
    |} with Y {
    |  val z = 7
    |}""")
    
  @Test def testTraitWithEarly5 = assertPrintedCode(sm"""
    |trait X extends {
    |  override protected[this] val x: Int = 5;
    |  val y: Double = 4.0;
    |  private type Foo;
    |  private[ee] type XString = String
    |} with Y {
    |  val z = 7
    |}""")
    
  @Test def testTraitWithSingletonTypeTree = assertPrintedCode(sm"""
    |trait Test {
    |  def testReturnSingleton(): this.type
    |}""")
    
  @Test def testTraitWithThis = assertPrintedCode(sm"""
    |trait Test { _ : X with Y =>
    |  
    |}""", q"trait Test { this: X with Y => }")
    
  @Test def testTraitWithWhile1 = assertPrintedCode(sm"""
    |trait Test {
    |  while (true.!=(false)) 
    |    println("testing...")
    |  
    |}""")
    
  @Test def testTraitWithWhile2 = assertPrintedCode(sm"""
    |trait Test {
    |  while (true) 
    |    {
    |      println("testing...");
    |      println("testing...")
    |    }
    |  
    |}""")
    
  @Test def testTraitWithDoWhile1 = assertPrintedCode(sm"""
    |trait Test {
    |  do 
    |    println("testing...")
    |   while (true) 
    |}""")  
    
  @Test def testTraitWithTypes = assertPrintedCode(sm"""
    |trait Test {
    |  type A = Int;
    |  type B >: Nothing <: AnyRef;
    |  protected type C >: Nothing;
    |  type D <: AnyRef
    |}""")   
}

trait ValAndDefPrintTests {
  @Test def testVal1 = assertPrintedCode("val a: Unit = null")
  
  @Test def testVal2 = assertPrintedCode("val * : Unit = null")
  
  @Test def testVal3 = assertPrintedCode("val a_ : Unit = null")
  
  @Test def testDef1 = assertPrintedCode("def a: Unit = null")
  
  @Test def testDef2 = assertPrintedCode("def * : Unit = null")
  
  @Test def testDef3 = assertPrintedCode("def a_(x: Int): Unit = null")
  
  @Test def testDef4 = assertPrintedCode("def a_ : Unit = null")
  
  @Test def testDef5 = assertPrintedCode("def a_(* : Int): Unit = null")
  
  @Test def testDef6 = assertPrintedCode("def a_(b_ : Int): Unit = null")
  
  @Test def testDef7 = assertPrintedCode(sm"""
    |{
    |  def test1 = ();
    |  def test2() = ()
    |}""", 
    Block(
      DefDef(NoMods, newTermName("test1"), Nil, Nil, EmptyTree, Literal(Constant(()))),
      DefDef(NoMods, newTermName("test2"), Nil, Nil :: Nil, EmptyTree, Literal(Constant(())))
    )
  )
  
  @Test def testDef8 = {
    val arg = ValDef(Modifiers(Flag.IMPLICIT) , newTermName("a"),
      AppliedTypeTree(Ident(newTypeName("R")), List(Ident(newTypeName("X")))), EmptyTree)
        
    //def m[X](implicit a: R[X]) = ()  
    val tree = DefDef(NoMods, newTermName("test"), TypeDef(NoMods, newTypeName("X"), Nil, EmptyTree) :: Nil,
      List(List(arg)), EmptyTree, Literal(Constant(())))

    assertPrintedCode("def test[X](implicit a: R[X]) = ()", tree)
  }
  
  @Test def testDefWithParams1 = assertPrintedCode("def foo(x: Int*) = null")
  
  @Test def testDefWithParams2 = assertPrintedCode("def foo(x: Int)(y: Int = 1) = null")
  
  @Test def testDefWithTypeParams1 = assertPrintedCode("def foo[A, B, C](x: A)(y: Int = 1): C = null")
  
  @Test def testDefWithTypeParams2 = assertPrintedCode("def foo[A, B <: Bar] = null")

  @Test def testDefWithAnn1 = assertPrintedCode("@annot def foo = null")
  
  @Test def testDefWithAnn2 = assertPrintedCode("@a(x) def foo = null")

  @Test def testDefWithAnn3 = assertPrintedCode("@Foo[A, B] def foo = null")
  
  @Test def testDefWithAnn4 = assertPrintedCode("@Foo(a)(b)(x, y) def foo = null")
  
  @Test def testDefWithAnn5 = assertPrintedCode("@Foo[A, B](a)(b) @Bar def foo(x: Int) = null")
  
  @Test def testDefWithAnn6 = assertPrintedCode("@test1(new test2()) def foo = 42")
  
  @Test def testDefWithAnn7 = assertPrintedCode("@`t*` def foo = 42")
  
  @Test def testDefWithAnn8 = assertPrintedCode("@throws(classOf[Exception]) def foo = throw new Exception()")
  
  @Test def testAnnotated1 = assertPrintedCode("def foo = 42: @test1")
  
  @Test def testAnnotated2 = assertPrintedCode("""def foo = 42: @test1(42, z = "5")""")
  
  @Test def testAnnotated3 = assertPrintedCode("def foo = (42: @test1): @test2(new test1())")
  
  @Test def testAnnotated4 = assertPrintedCode("""def foo = 42: @test1(4, "testing")(4.2)""")
    
  @Test def testAnnotated5 = assertPrintedCode("""def foo = (42: @test1(4, "testing")(4.2)): @test2(1, "bar")(3.14)""")
  
  @Test def testAnnotated6 = assertPrintedCode("def foo = ((42: @test1): @test2(new test1())): @test3(1)(2, 3)(4)")
  
  @Test def testAnnotated7 = assertPrintedCode(sm"""
    |(x: @unchecked) match {
    |  case ((_): Int) => true
    |  case _ => false
    |}""")
  
  @Test def testAnnotated8 = assertPrintedCode(sm"""
    |((x: @unchecked): @test1(1, "testing")(3.14)) match {
    |  case _ => true
    |}""")
}

trait PackagePrintTests {
  @Test def testPackage1 = assertPrintedCode(sm"""
    |package foo.bar {
    |  
    |}""")
  
  @Test def testPackage2 = assertPrintedCode(sm"""
    |package foo {
    |  class C
    |
    |  object D
    |}""")

  //package object foo extends a with b
  @Test def testPackage3 = assertPrintedCode(sm"""
    |package foo {
    |  object `package` extends a with b
    |}""")

  //package object foo { def foo; val x = 1 }
  @Test def testPackage4 = assertPrintedCode(sm"""
    |package foo {
    |  object `package` {
    |    def foo: scala.Unit;
    |    val x = 1
    |  }
    |}""")

  //package object foo extends { val x = 1; type I = Int } with Any
  @Test def testPackage5 = assertPrintedCode(sm"""
    |package foo {
    |  object `package` extends {
    |    val x = 1;
    |    type I = Int
    |  } with Any
    |}""")
}

trait QuasiTreesPrintTests {  
  @Test def testQuasiIdent = assertPrintedCode("*", q"*")
  
  @Test def testQuasiVal = assertPrintedCode("val * : Unit = null", q"val * : Unit = null")
  
  @Test def testQuasiDef = assertPrintedCode("def * : Unit = null", q"def * : Unit = null")
  
  @Test def testQuasiTrait = assertPrintedCode("trait *", q"trait *")
  
  @Test def testQuasiClass = assertPrintedCode("class *", q"class *")
  
  @Test def testQuasiClassWithPublicParams = assertPrintedCode( "class X(val x: Int, val s: String)", q"class X(val x: Int, val s:String)" )
  
  @Test def testQuasiClassWithParams = assertPrintedCode("class X(x: Int, s: String)", q"class X(x: Int, s:String)")
  
  @Test def testQuasiObject = assertPrintedCode("object *", q"object *")
  
  @Test def testQuasiObjectWithBody = assertPrintedCode(sm"""
    |object X {
    |  def y = "test"
    |}""", q"""object X{ def y = "test" }""")

  @Test def testQuasiClassWithBody = assertPrintedCode(sm"""
    |class X {
    |  def y = "test"
    |}""", q"""class X{ def y = "test" }""")

  @Test def testQuasiTraitWithBody = assertPrintedCode(sm"""
    |trait X {
    |  def y = "test"
    |}""", q"""trait X{ def y = "test" }""")

  @Test def testQuasiTraitWithSelfTypeAndBody = assertPrintedCode(sm"""
    |trait X { self: Order =>
    |  def y = "test"
    |}""", q"""trait X{ self: Order => def y = "test" }""")

  @Test def testQuasiTraitWithSelf = assertPrintedCode(sm"""
    |trait X { self =>
    |  def y = "test"
    |}""", q"""trait X{ self => def y = "test" }""")

  @Test def testQuasiCaseClassWithBody = assertPrintedCode(sm"""
    |case class X() {
    |  def y = "test"
    |}""", q"""case class X() { def y = "test" }""")

  @Test def testQuasiCaseClassWithParamsAndBody = assertPrintedCode(sm"""
    |case class X(x: Int, s: String) {
    |  def y = "test"
    |}""", q"""case class X(x: Int, s: String){ def y = "test" }""")
}
