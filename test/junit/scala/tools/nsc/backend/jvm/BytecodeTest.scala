package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test

import scala.annotation.unused
import scala.jdk.CollectionConverters._
import scala.tools.asm.Opcodes._
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.MethodNode

class BytecodeTest extends BytecodeTesting {
  import compiler._

  @Test
  def t10812(): Unit = {
    def code(prefix: String) =
      s"""$prefix A { def f: Object = null }
        |object B extends A { override def f: String = "b" }
      """.stripMargin
    for (base <- List("trait", "class")) {
      val List(a, bMirror, bModule) = compileClasses(code(base))
      assertEquals(bMirror.name, "B")
      assertEquals(bMirror.methods.asScala.filter(_.name == "f").map(m => m.name + m.desc).toList, List("f()Ljava/lang/String;"))
    }
  }

  @Test
  def t6288bJumpPosition(): Unit = {
    val code =
      """object Case3 {                                 // 01
        | def unapply(z: Any): Option[Int] = Some(-1)   // 02
        | def main(args: Array[String]): Unit = {       // 03
        |    ("": Any) match {                          // 04
        |      case x : String =>                       // 05
        |        println("case 0")                      // 06 println and jump at 6
        |      case _ =>                                // 07
        |        println("default")                     // 08 println and jump at 8
        |    }                                          // 09
        |    println("done")                            // 10
        |  }
        |}
      """.stripMargin
    val List(mirror, module) = compileClasses(code)

    val unapplyLineNumbers = getInstructions(module, "unapply").filter(_.isInstanceOf[LineNumber])
    assert(unapplyLineNumbers == List(LineNumber(2, Label(0))), unapplyLineNumbers)

    val expected = List(
      LineNumber(4, Label(0)),
      LineNumber(5, Label(5)),
      Jump(IFEQ, Label(20)),

      LineNumber(6, Label(11)),
      InvokeVirtual("scala/Predef$", "println", "(Ljava/lang/Object;)V"),
      Jump(GOTO, Label(33)),

      LineNumber(5, Label(20)),
      Jump(GOTO, Label(24)),

      LineNumber(8, Label(24)),
      InvokeVirtual("scala/Predef$", "println", "(Ljava/lang/Object;)V"),
      Jump(GOTO, Label(33)),

      LineNumber(10, Label(33)),
      InvokeVirtual("scala/Predef$", "println", "(Ljava/lang/Object;)V")
    )

    val mainIns = getInstructions(module, "main") filter {
      case _: LineNumber | _: Invoke | _: Jump => true
      case _ => false
    }
    assertSameCode(mainIns, expected)
  }

  @Test
  def bytecodeForBranches(): Unit = {
    val code =
      """class C {
        |  def t1(b: Boolean) = if (b) 1 else 2
        |  def t2(x: Int) = if (x == 393) 1 else 2
        |  def t3(a: Array[String], b: AnyRef) = a != b && b == a
        |  def t4(a: AnyRef) = a == null || null != a
        |  def t5(a: AnyRef) = (a eq null) || (null ne a)
        |  def t6(a: Int, b: Boolean) = if ((a == 10) && b || a != 1) 1 else 2
        |  def t7(a: AnyRef, b: AnyRef) = a == b
        |  def t8(a: AnyRef) = scala.collection.immutable.Nil == a || "" != a
        |}
      """.stripMargin

    val c = compileClass(code)

    // t1: no unnecessary GOTOs
    assertSameCode(getMethod(c, "t1"), List(
      VarOp(ILOAD, 1), Jump(IFEQ, Label(6)),
      Op(ICONST_1), Op(IRETURN),
      Label(6), Op(ICONST_2), Op(IRETURN)))

    // t2: no unnecessary GOTOs
    assertSameCode(getMethod(c, "t2"), List(
      VarOp(ILOAD, 1), IntOp(SIPUSH, 393), Jump(IF_ICMPNE, Label(7)),
      Op(ICONST_1), Op(IRETURN),
      Label(7), Op(ICONST_2), Op(IRETURN)))

    // t3: Array == is translated to reference equality, AnyRef == to null checks and equals
    assertSameCode(getMethod(c, "t3"), List(
      // Array ==
      VarOp(ALOAD, 1), VarOp(ALOAD, 2), Jump(IF_ACMPEQ, Label(23)),
      // AnyRef ==
      VarOp(ALOAD, 2), VarOp(ALOAD, 1), VarOp(ASTORE, 3), Op(DUP), Jump(IFNONNULL, Label(14)),
      Op(POP), VarOp(ALOAD, 3), Jump(IFNULL, Label(19)), Jump(GOTO, Label(23)),
      Label(14), VarOp(ALOAD, 3), InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z"), Jump(IFEQ, Label(23)),
      Label(19), Op(ICONST_1), Jump(GOTO, Label(26)),
      Label(23), Op(ICONST_0),
      Label(26), Op(IRETURN)))

    val t4t5 = List(
      VarOp(ALOAD, 1), Jump(IFNULL, Label(6)),
      VarOp(ALOAD, 1), Jump(IFNULL, Label(10)),
      Label(6), Op(ICONST_1), Jump(GOTO, Label(13)),
      Label(10), Op(ICONST_0),
      Label(13), Op(IRETURN))

    // t4: one side is known null, so just a null check on the other
    assertSameCode(getMethod(c, "t4"), t4t5)

    // t5: one side known null, so just a null check on the other
    assertSameCode(getMethod(c, "t5"), t4t5)

    // t6: no unnecessary GOTOs
    assertSameCode(getMethod(c, "t6"), List(
      VarOp(ILOAD, 1), IntOp(BIPUSH, 10), Jump(IF_ICMPNE, Label(7)),
      VarOp(ILOAD, 2), Jump(IFNE, Label(12)),
      Label(7), VarOp(ILOAD, 1), Op(ICONST_1), Jump(IF_ICMPEQ, Label(16)),
      Label(12), Op(ICONST_1), Op(IRETURN),
      Label(16), Op(ICONST_2), Op(IRETURN)))

    // t7: universal equality
    assertInvoke(getMethod(c, "t7"), "scala/runtime/BoxesRunTime", "equals")

    // t8: no null checks invoking equals on modules and constants
    assertSameCode(getMethod(c, "t8"), List(
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"), VarOp(ALOAD, 1), InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z"), Jump(IFNE, Label(10)),
      Ldc(LDC, ""), VarOp(ALOAD, 1), InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z"), Jump(IFNE, Label(14)),
      Label(10), Op(ICONST_1), Jump(GOTO, Label(17)),
      Label(14), Op(ICONST_0),
      Label(17), Op(IRETURN)))
  }

  @Test // wrong local variable table for methods containing while loops
  def t9179(): Unit = {
    val code =
      """class C {
        |  def t(): Unit = {
        |    var x = ""
        |    while (x != null) {
        |      foo()
        |      x = null
        |    }
        |    bar()
        |  }
        |  def foo(): Unit = ()
        |  def bar(): Unit = ()
        |}
      """.stripMargin
    val c = compileClass(code)
    val t = getMethod(c, "t")
    val isFrameLine = (x: Instruction) => x.isInstanceOf[FrameEntry] || x.isInstanceOf[LineNumber]
    assertSameCode(t.instructions.filterNot(isFrameLine), List(
      Label(0), Ldc(LDC, ""), VarOp(ASTORE, 1),
      Label(4), VarOp(ALOAD, 1), Jump(IFNULL, Label(20)),
      Label(9), VarOp(ALOAD, 0), InvokeVirtual("C", "foo", "()V"), Label(13), Op(ACONST_NULL), VarOp(ASTORE, 1), Label(17), Jump(GOTO, Label(4)),
      Label(20), VarOp(ALOAD, 0), InvokeVirtual("C", "bar", "()V"), Op(RETURN), Label(26)))
    val labels = t.instructions collect { case l: Label => l }
    val x = t.localVars.find(_.name == "x").get
    assertEquals(x.start, labels(1))
    assertEquals(x.end, labels(6))
  }

  @Test
  def sd186_traitLineNumber(): Unit = {
    val code =
      """trait T {
        |  def t(): Unit = {
        |    toString
        |    toString
        |  }
        |}
      """.stripMargin
    val t = compileClass(code)
    val tMethod = getMethod(t, "t$")
    @unused val invoke = InvokeVirtual("java/lang/Object", "toString", "()Ljava/lang/String;")
    // ths static accessor is positioned at the line number of the accessed method.
    assertSameCode(tMethod.instructions,
      List(Label(0), LineNumber(2, Label(0)), VarOp(ALOAD, 0), Invoke(INVOKESPECIAL, "T", "t", "()V", itf = true), Op(RETURN), Label(4))
    )
  }

  @Test def `class constructor has correct line numbers (12470)`: Unit = {
    val code =
      """class A
        |class B
        |object D
        |class C
      """.stripMargin
    val lines = Map("A" -> 1, "B" -> 2, "D$" -> 3, "C" -> 4)
    compileClasses(code).foreach { c =>
      c.methods.asScala.foreach(m => convertMethod(m).instructions.foreach {
        case LineNumber(n, _) => assertEquals(s"class ${c.name} method ${m.name}", lines(c.name), n)
        case _ =>
      })
    }
  }

  @Test
  def sd233(): Unit = {
    val code = "def f = { println(1); synchronized(println(2)) }"
    val m = compileMethod(code)
    val List(ExceptionHandler(_, _, _, desc)) = m.handlers
    assert(desc == None, desc)
  }

  @Test
  def classesEndingInDollarHaveSignature(): Unit = {
    // A name-based test in the backend prevented classes ending in $ from getting a Scala signature
    val code = "class C$"
    val c = compileClass(code)
    assertEquals(c.attrs.asScala.toList.map(_.`type`).sorted, List("ScalaInlineInfo", "ScalaSig"))
  }

  @Test
  def t10343(): Unit = {
    val main = "class Main { Person() }"
    val person = "case class Person(age: Int = 1)"

    def check(code: String) = {
      val List(_, _, pm) = compileClasses(code)
      assertEquals(pm.name, "Person$")
      assertEquals(pm.methods.asScala.map(_.name).toList,
        // after typer, `"$lessinit$greater$default$1"` is next to `<init>`, but the constructor phase
        // and code gen change module constructors around. the second `apply` is a bridge, created in erasure.
        List("<clinit>", "$lessinit$greater$default$1", "toString", "apply", "apply$default$1", "unapply", "writeReplace", "apply", "<init>"))
    }
    check(s"$main\n$person")
    check(s"$person\n$main")
  }

  @Test
  def t11127(): Unit = {
    val code =
      """abstract class C {
        |  def b: Boolean
        |
        |  // no need to lift the `try` to a separate method if it's in the receiver expression
        |  def t1 = (try { Console } catch { case _: ClassCastException => Console }).println()
        |
        |  // no need to lift the `try`
        |  def t2 = !(try b catch { case _: ClassCastException => b })
        |
        |  def t3 = b || (try b catch { case _: ClassCastException => b })
        |
        |  def t4 = (try b catch { case _: ClassCastException => b }) || b
        |
        |  def t5 = b && (try b catch { case _: ClassCastException => b })
        |
        |  def t6 = (try b catch { case _: ClassCastException => b }) && b
        |
        |  def t7 = (try b catch { case _: ClassCastException => b }) && b || b && (try b catch { case _: ClassCastException => b || (try b catch { case _: ClassCastException => b }) })
        |}
      """.stripMargin
    val c = compileClass(code)
    def check(m: String, invoked: List[String]) = {
      val meth = getMethod(c, m)
      assert(meth.handlers.nonEmpty, meth.handlers)
      assertInvokedMethods(meth, invoked)
    }
    check("t1", List("scala/Console$.println"))
    check("t2", List("C.b", "C.b"))
    check("t3", List("C.b", "C.b", "C.b"))
    check("t4", List("C.b", "C.b", "C.b"))
    check("t5", List("C.b", "C.b", "C.b"))
    check("t6", List("C.b", "C.b", "C.b"))
    check("t7", List("C.b", "C.b", "C.b", "C.b", "C.b", "C.b", "C.b", "C.b"))
  }

  @Test
  def t11412(): Unit = {
    val code = "class A { val a = 0 }; class C extends A with App { val x = 1; val y = x }"
    val cs = compileClasses(code)
    val c = cs.find(_.name == "C").get
    val fs = c.fields.asScala.toList.sortBy(_.name).map(f => (f.name, (f.access & Opcodes.ACC_FINAL) != 0))
    assertEquals(List(
      ("executionStart", false),
      ("scala$App$$_args", false),
      ("scala$App$$initCode", false),
      ("x", false),
      ("y", false)
    ), fs)
    val assignedInConstr = getMethod(c, "<init>").instructions.filter(_.opcode == Opcodes.PUTFIELD)
    assertEquals(Nil, assignedInConstr)
  }

  @Test
  def t11412b(): Unit = {
    val code = "class C { def f = { var x = 0; val y = 1; class K extends App { def m = x + y } } }"
    val cs = compileClasses(code)
    val k = cs.find(_.name == "C$K$1").get
    val fs = k.fields.asScala.toList.sortBy(_.name).map(f => (f.name, (f.access & Opcodes.ACC_FINAL) != 0))
    assertEquals(List(
      ("$outer", true),
      ("executionStart", false),
      ("scala$App$$_args", false),
      ("scala$App$$initCode", false),
      ("x$1", true), // captured, assigned in constructor
      ("y$1", true)  // captured
    ), fs)
    val assignedInConstr = getMethod(k, "<init>").instructions.collect {
      case f: Field if f.opcode == Opcodes.PUTFIELD => f.name
    }
    assertEquals(List("$outer", "x$1", "y$1"), assignedInConstr.sorted)
  }

  @Test
  def t11641(): Unit = {
    val code =
      """class B { val b = 0 }
        |class C extends DelayedInit {
        |  def delayedInit(body: => Unit): Unit = ()
        |}
        |class D extends DelayedInit {
        |  val d = 0
        |  def delayedInit(body: => Unit): Unit = ()
        |}
        |class E extends C {
        |  val e = 0
        |}
        |class F extends D
      """.stripMargin
    val cs = compileClasses(code, allowMessage = _.msg.contains("2 deprecations"))

    assertDoesNotInvoke(getMethod(cs.find(_.name == "B").get, "<init>"), "releaseFence")
    assertDoesNotInvoke(getMethod(cs.find(_.name == "C").get, "<init>"), "releaseFence")
    assertInvoke(getMethod(cs.find(_.name == "D").get, "<init>"), "scala/runtime/Statics", "releaseFence")
    assertInvoke(getMethod(cs.find(_.name == "E").get, "<init>"), "scala/runtime/Statics", "releaseFence")
    assertDoesNotInvoke(getMethod(cs.find(_.name == "F").get, "<init>"), "releaseFence")
  }

  @Test
  def t11718(): Unit = {
    val code = """class A11718 { private val a = ""; lazy val b = a }"""
    val cs = compileClasses(code)
    val A = cs.find(_.name == "A11718").get
    val a = A.fields.asScala.find(_.name == "a").get
    assertEquals(0, a.access & Opcodes.ACC_FINAL)
  }

  @Test
  def t12362(): Unit = {
    val code                 =
      """object Test {
        |  def foo(value: String) = {
        |    println(value)
        |  }
        |
        |  def abcde(value1: String, value2: Long, value3: Double, value4: Int, value5: Double): Double = {
        |    println(value1)
        |    value5
        |  }
        |}""".stripMargin

    val List(mirror, _) = compileClasses(code)
    assertEquals(mirror.name, "Test")

    val foo    = getAsmMethod(mirror, "foo")
    val abcde  = getAsmMethod(mirror, "abcde")

    def t(m: MethodNode, r: List[(String, String, Int)]) = {
      assertTrue((m.access & Opcodes.ACC_STATIC) != 0)
      assertEquals(r, m.localVariables.asScala.toList.map(l => (l.desc, l.name, l.index)))
    }

    t(foo, List(("Ljava/lang/String;", "value", 0)))
    t(abcde, List(("Ljava/lang/String;", "value1", 0), ("J", "value2", 1), ("D", "value3", 3), ("I", "value4", 5), ("D", "value5", 6)))
  }

  @Test
  def nonSpecializedValFence(): Unit = {
    def code(u1: String) =
      s"""abstract class Speck[@specialized(Int) T](t: T, sm: String, val sn: String) {
         |  val a = t
         |  $u1
         |  lazy val u2 = "?"
         |  var u3 = "?"
         |  val u4: String
         |  var u5: String
         |}
         |""".stripMargin

    for (u1 <- "" :: List("", "private", "private[this]", "protected").map(mod => s"$mod val u1 = \"?\"")) {
      for (c <- compileClasses(code(u1)).map(getMethod(_, "<init>")))
        if (u1.isEmpty)
          assertDoesNotInvoke(c, "releaseFence")
        else
          assertInvoke(c, "scala/runtime/Statics", "releaseFence")
    }
  }

  @Test
  def t12340(): Unit = {
    val code =
      """class C {
        |  def foo(param: String) = {
        |    trait T {
        |      def bar = param
        |    }
        |    (new T {}).bar
        |  }
        |}""".stripMargin
    val List(_, cAnon, _) = compileClasses(code)
    // field for caputred param is not final
    assertEquals(Opcodes.ACC_PRIVATE, cAnon.fields.asScala.find(_.name.startsWith("param")).get.access)
    assertInvoke(getMethod(cAnon, "<init>"), "scala/runtime/Statics", "releaseFence")
  }

    @Test
    def t12881(): Unit = {
      val code =
        """trait T {
          |  val foo = "foo"
          |}
          |
          |class C {
          |  def f: T = {
          |    lazy val t = new T {}
          |    t
          |  }
          |}
          |""".stripMargin
      val List(_, cAnon, _) = compileClasses(code)
      // mixed-in field is not final
      assertEquals(Opcodes.ACC_PRIVATE, cAnon.fields.asScala.find(_.name.startsWith("foo")).get.access)
      assertInvoke(getMethod(cAnon, "<init>"), "scala/runtime/Statics", "releaseFence")
    }

  @Test def tailrecControlFlow(): Unit = {
    // Without change of the `this` value

    val sourceFoo =
      s"""class Foo {
         |  @scala.annotation.tailrec // explicit @tailrec here
         |  final def fact(n: Int, acc: Int): Int =
         |    if (n == 0) acc
         |    else fact(n - 1, acc * n)
         |}
         """.stripMargin

    val fooClass = compileClass(sourceFoo)

    assertSameCode(getMethod(fooClass, "fact"), List(
      Label(0),
      VarOp(ILOAD, 1),
      Op(ICONST_0),
      Jump(IF_ICMPNE, Label(8)),
      VarOp(ILOAD, 2),
      Op(IRETURN),
      Label(8),
      VarOp(ILOAD, 1),
      Op(ICONST_1),
      Op(ISUB),
      VarOp(ILOAD, 2),
      VarOp(ILOAD, 1),
      Op(IMUL),
      VarOp(ISTORE, 2),
      VarOp(ISTORE, 1),
      Jump(GOTO, Label(0)),
    ))

    // With changing the `this` value

    val sourceIntList =
      s"""class IntList(head: Int, tail: IntList) {
         |  // implicit @tailrec
         |  final def sum(acc: Int): Int = {
         |    val t = tail
         |    if (t == null) acc + head
         |    else t.sum(acc + head)
         |  }
         |}
         """.stripMargin

    val intListClass = compileClass(sourceIntList)

    assertSameCode(getMethod(intListClass, "sum"), List(
      Label(0),
      VarOp(ALOAD, 0),
      Field(GETFIELD, "IntList", "tail", "LIntList;"),
      VarOp(ASTORE, 3),
      VarOp(ALOAD, 3),
      Jump(IFNONNULL, Label(15)),
      VarOp(ILOAD, 1),
      VarOp(ALOAD, 0),
      Field(GETFIELD, "IntList", "head", "I"),
      Op(IADD),
      Op(IRETURN),
      Label(15),
      VarOp(ALOAD, 3),
      VarOp(ILOAD, 1),
      VarOp(ALOAD, 0),
      Field(GETFIELD, "IntList", "head", "I"),
      Op(IADD),
      VarOp(ISTORE, 1),
      VarOp(ASTORE, 0),
      Jump(GOTO, Label(0)),
    ))
  }

  @Test def patmatControlFlow(): Unit = {
    val source =
      s"""class Foo {
         |  def m1(xs: List[Int]): Int = xs match {
         |    case x :: xr => x
         |    case Nil     => 20
         |  }
         |
         |  def m2(xs: List[Int]): Int = xs match {
         |    case (1 | 2) :: xr => 10
         |    case x :: xr => x
         |    case _ => 20
         |  }
         |
         |  def m3(xs: List[Int]): Unit = xs match {
         |    case x :: _ => println(x)
         |    case Nil    => println("nil")
         |  }
         |
         |  def m4(xs: List[Int]): Int = return xs match {
         |    case x :: xr => x
         |    case Nil     => 20
         |  }
         |}
         """.stripMargin

    val fooClass = compileClass(source)

    // ---------------

    assertSameCode(getMethod(fooClass, "m1"), List(
      VarOp(ALOAD, 1),
      VarOp(ASTORE, 3),
      VarOp(ALOAD, 3),
      TypeOp(INSTANCEOF, "scala/collection/immutable/$colon$colon"),
      Jump(IFEQ, Label(19)),
      VarOp(ALOAD, 3),
      TypeOp(CHECKCAST, "scala/collection/immutable/$colon$colon"),
      VarOp(ASTORE, 4),
      VarOp(ALOAD, 4),
      InvokeVirtual("scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;"),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", itf = false),
      VarOp(ISTORE, 5),
      VarOp(ILOAD, 5),
      Op(IRETURN),
      Label(19),
      Jump(GOTO, Label(22)),
      Label(22),
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"),
      VarOp(ALOAD, 3),
      InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z"),
      Jump(IFEQ, Label(31)),
      IntOp(BIPUSH, 20),
      Op(IRETURN),
      Label(31),
      Jump(GOTO, Label(34)),
      Label(34),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ALOAD, 3),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", itf = false),
      Op(ATHROW),
    ))

    // ---------------

    assertSameCode(getMethod(fooClass, "m2"), List(
      Op(ICONST_0),
      VarOp(ISTORE, 4),
      Op(ACONST_NULL),
      VarOp(ASTORE, 5),
      VarOp(ALOAD, 1),
      VarOp(ASTORE, 6),
      VarOp(ALOAD, 6),
      TypeOp(INSTANCEOF, "scala/collection/immutable/$colon$colon"),
      Jump(IFEQ, Label(56)),
      Op(ICONST_1),
      VarOp(ISTORE, 4),
      VarOp(ALOAD, 6),
      TypeOp(CHECKCAST, "scala/collection/immutable/$colon$colon"),
      VarOp(ASTORE, 5),
      VarOp(ALOAD, 5),
      InvokeVirtual("scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;"),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", itf = false),
      VarOp(ISTORE, 7),
      Op(ICONST_1),
      VarOp(ILOAD, 7),
      Jump(IF_ICMPNE, Label(28)),
      Op(ICONST_1),
      Jump(GOTO, Label(44)),
      Label(28),
      Jump(GOTO, Label(31)),
      Label(31),
      Op(ICONST_2),
      VarOp(ILOAD, 7),
      Jump(IF_ICMPNE, Label(39)),
      Op(ICONST_1),
      Jump(GOTO, Label(44)),
      Label(39),
      Jump(GOTO, Label(42)),
      Label(42),
      Op(ICONST_0),
      Jump(GOTO, Label(44)),
      Label(44),
      Jump(IFEQ, Label(53)),
      IntOp(BIPUSH, 10),
      Op(IRETURN),
      Label(53),
      Jump(GOTO, Label(59)),
      Label(56),
      Jump(GOTO, Label(59)),
      Label(59),
      VarOp(ILOAD, 4),
      Jump(IFEQ, Label(71)),
      VarOp(ALOAD, 5),
      InvokeVirtual("scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;"),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", itf = false),
      VarOp(ISTORE, 8),
      VarOp(ILOAD, 8),
      Op(IRETURN),
      Label(71),
      Jump(GOTO, Label(74)),
      Label(74),
      IntOp(BIPUSH, 20),
      Op(IRETURN),
    ))

    // ---------------

    assertSameCode(getMethod(fooClass, "m3"), List(
      VarOp(ALOAD, 1),
      VarOp(ASTORE, 3),
      VarOp(ALOAD, 3),
      TypeOp(INSTANCEOF, "scala/collection/immutable/$colon$colon"),
      Jump(IFEQ, Label(24)),
      VarOp(ALOAD, 3),
      TypeOp(CHECKCAST, "scala/collection/immutable/$colon$colon"),
      VarOp(ASTORE, 4),
      VarOp(ALOAD, 4),
      InvokeVirtual("scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;"),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", itf = false),
      VarOp(ISTORE, 5),
      Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
      VarOp(ILOAD, 5),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", itf = false),
      InvokeVirtual("scala/Predef$", "println", "(Ljava/lang/Object;)V"),
      Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"),
      Op(POP),
      Op(RETURN),
      Label(24),
      Jump(GOTO, Label(27)),
      Label(27),
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"),
      VarOp(ALOAD, 3),
      InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z"),
      Jump(IFEQ, Label(40)),
      Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
      Ldc(LDC, "nil"),
      InvokeVirtual("scala/Predef$", "println", "(Ljava/lang/Object;)V"),
      Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"),
      Op(POP),
      Op(RETURN),
      Label(40),
      Jump(GOTO, Label(43)),
      Label(43),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ALOAD, 3),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", itf = false),
      Op(ATHROW),
    ))

    // ---------------

    assertSameCode(getMethod(fooClass, "m4"), List(
      VarOp(ALOAD, 1),
      VarOp(ASTORE, 3),
      VarOp(ALOAD, 3),
      TypeOp(INSTANCEOF, "scala/collection/immutable/$colon$colon"),
      Jump(IFEQ, Label(19)),
      VarOp(ALOAD, 3),
      TypeOp(CHECKCAST, "scala/collection/immutable/$colon$colon"),
      VarOp(ASTORE, 4),
      VarOp(ALOAD, 4),
      InvokeVirtual("scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;"),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", itf = false),
      VarOp(ISTORE, 5),
      VarOp(ILOAD, 5),
      Op(IRETURN),
      Label(19),
      Jump(GOTO, Label(22)),
      Label(22),
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"),
      VarOp(ALOAD, 3),
      InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z"),
      Jump(IFEQ, Label(31)),
      IntOp(BIPUSH, 20),
      Op(IRETURN),
      Label(31),
      Jump(GOTO, Label(34)),
      Label(34),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ALOAD, 3),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", itf = false),
      Op(ATHROW),
    ))

    // ---------------

    val sourcePatmatAdaptMatchEnd =
      s"""class Tree
         |
         |trait RefTree extends Tree
         |
         |class A extends RefTree
         |class B extends RefTree
         |
         |class PatmatAdaptMatchEnd {
         |  def atPos[T <: Tree](tree: T): T = tree
         |
         |  def test(xs: List[Int]): Tree = {
         |    val tree1 = atPos {
         |      xs match {
         |        case head :: tail => new A
         |        case Nil          => new B
         |      }
         |    }
         |    tree1
         |  }
         |}
         """.stripMargin

    val List(cA, cB, cPatmatAdaptMatchEnd, cRefTree, cTree) = compileClasses(sourcePatmatAdaptMatchEnd)

    assertSameCode(getMethod(cPatmatAdaptMatchEnd, "test"), List(
      VarOp(ALOAD, 0),
      VarOp(ALOAD, 1),
      VarOp(ASTORE, 4),
      VarOp(ALOAD, 4),
      TypeOp(INSTANCEOF, "scala/collection/immutable/$colon$colon"),
      Jump(IFEQ, Label(16)),
      TypeOp(NEW, "A"),
      Op(DUP),
      Invoke(INVOKESPECIAL, "A", "<init>", "()V", itf = false),
      Jump(GOTO, Label(41)),
      Label(16),
      Jump(GOTO, Label(19)),
      Label(19),
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"),
      VarOp(ALOAD, 4),
      InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z"),
      Jump(IFEQ, Label(30)),
      TypeOp(NEW, "B"),
      Op(DUP),
      Invoke(INVOKESPECIAL, "B", "<init>", "()V", itf = false),
      Jump(GOTO, Label(41)),
      Label(30),
      Jump(GOTO, Label(33)),
      Label(33),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ALOAD, 4),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", itf = false),
      Op(ATHROW),
      Label(41),
      TypeOp(CHECKCAST, "Tree"),
      InvokeVirtual("PatmatAdaptMatchEnd", "atPos", "(LTree;)LTree;"),
      TypeOp(CHECKCAST, "RefTree"),
      VarOp(ASTORE, 3),
      VarOp(ALOAD, 3),
      TypeOp(CHECKCAST, "Tree"),
      Op(ARETURN),
    ))
  }

  @Test def switchControlFlow(): Unit = {
    val source =
      s"""import scala.annotation.switch
         |
         |class Foo {
         |  def m1(x: Int): Int = (x: @switch) match {
         |    case 1 => 10
         |    case 7 => 20
         |    case 8 => 30
         |    case 9 => 40
         |    case _ => x
         |  }
         |
         |  def m2(x: Int): Int = (x: @switch) match {
         |    case (1 | 2)      => 10
         |    case 7            => 20
         |    case 8            => 30
         |    case c if c > 100 => 20
         |  }
         |}
         """.stripMargin

    val fooClass = compileClass(source)

    // ---------------

    assertSameCode(getMethod(fooClass, "m1"), List(
      VarOp(ILOAD, 1),
      VarOp(ISTORE, 2),
      VarOp(ILOAD, 2),
      LookupSwitch(LOOKUPSWITCH, Label(26), List(1, 7, 8, 9), List(Label(6), Label(11), Label(16), Label(21))),
      Label(6),
      IntOp(BIPUSH, 10),
      Op(IRETURN),
      Label(11),
      IntOp(BIPUSH, 20),
      Op(IRETURN),
      Label(16),
      IntOp(BIPUSH, 30),
      Op(IRETURN),
      Label(21),
      IntOp(BIPUSH, 40),
      Op(IRETURN),
      Label(26),
      VarOp(ILOAD, 1),
      Op(IRETURN),
    ))

    // ---------------

    assertSameCode(getMethod(fooClass, "m2"), List(
      VarOp(ILOAD, 1),
      VarOp(ISTORE, 2),
      VarOp(ILOAD, 2),
      LookupSwitch(LOOKUPSWITCH, Label(21), List(1, 2, 7, 8), List(Label(6), Label(6), Label(11), Label(16))),
      Label(6),
      IntOp(BIPUSH, 10),
      Op(IRETURN),
      Label(11),
      IntOp(BIPUSH, 20),
      Op(IRETURN),
      Label(16),
      IntOp(BIPUSH, 30),
      Op(IRETURN),
      Label(21),
      VarOp(ILOAD, 2),
      IntOp(BIPUSH, 100),
      Jump(IF_ICMPLE, Label(29)),
      IntOp(BIPUSH, 20),
      Op(IRETURN),
      Label(29),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ILOAD, 2),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", itf = false),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", itf = false),
      Op(ATHROW),
    ))
  }

  @Test def ifThenElseControlFlow(): Unit = {
    /* This is a test case coming from the Scala.js linker, where in Scala 2 we
     * had to introduce a "useless" `return` to make the bytecode size smaller,
     * measurably increasing performance (!).
     */

    val source =
      s"""import java.io.Writer
         |
         |final class SourceMapWriter(out: Writer) {
         |  private val Base64Map =
         |      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
         |      "abcdefghijklmnopqrstuvwxyz" +
         |      "0123456789+/"
         |
         |  private final val VLQBaseShift = 5
         |  private final val VLQBase = 1 << VLQBaseShift
         |  private final val VLQBaseMask = VLQBase - 1
         |  private final val VLQContinuationBit = VLQBase
         |
         |  def entryPoint(value: Int): Unit = writeBase64VLQ(value)
         |
         |  private def writeBase64VLQ(value0: Int): Unit = {
         |    val signExtended = value0 >> 31
         |    val value = (((value0 ^ signExtended) - signExtended) << 1) | (signExtended & 1)
         |    if (value < 26) {
         |      out.write('A' + value) // was `return out...`
         |    } else {
         |      def writeBase64VLQSlowPath(value0: Int): Unit = {
         |        var value = value0
         |        do {
         |          var digit = value & VLQBaseMask
         |          value = value >>> VLQBaseShift
         |          if (value != 0)
         |            digit |= VLQContinuationBit
         |          out.write(Base64Map.charAt(digit))
         |        } while (value != 0)
         |      }
         |      writeBase64VLQSlowPath(value)
         |    }
         |  }
         |}
         """.stripMargin

    val sourceMapWriterClass = compileClass(source)

    // ---------------

    assertSameCode(getMethod(sourceMapWriterClass, "writeBase64VLQ"), List(
      VarOp(ILOAD, 1),
      IntOp(BIPUSH, 31),
      Op(ISHR),
      VarOp(ISTORE, 2),
      VarOp(ILOAD, 1),
      VarOp(ILOAD, 2),
      Op(IXOR),
      VarOp(ILOAD, 2),
      Op(ISUB),
      Op(ICONST_1),
      Op(ISHL),
      VarOp(ILOAD, 2),
      Op(ICONST_1),
      Op(IAND),
      Op(IOR),
      VarOp(ISTORE, 3),
      VarOp(ILOAD, 3),
      IntOp(BIPUSH, 26),
      Jump(IF_ICMPGE, Label(34)),
      VarOp(ALOAD, 0),
      Field(GETFIELD, "SourceMapWriter", "out", "Ljava/io/Writer;"),
      IntOp(BIPUSH, 65),
      VarOp(ILOAD, 3),
      Op(IADD),
      InvokeVirtual("java/io/Writer", "write", "(I)V"),
      Op(RETURN),
      Label(34),
      VarOp(ALOAD, 0),
      VarOp(ILOAD, 3),
      Invoke(INVOKESPECIAL, "SourceMapWriter", "writeBase64VLQSlowPath$1", "(I)V", itf = false),
      Op(RETURN),
    ))

    // ---------------

    assertSameCode(getMethod(sourceMapWriterClass, "writeBase64VLQSlowPath$1"), List(
      VarOp(ILOAD, 1),
      VarOp(ISTORE, 2),
      Label(4),
      VarOp(ILOAD, 2),
      IntOp(BIPUSH, 31),
      Op(IAND),
      VarOp(ISTORE, 3),
      VarOp(ILOAD, 2),
      Op(ICONST_5),
      Op(IUSHR),
      VarOp(ISTORE, 2),
      VarOp(ILOAD, 2),
      Op(ICONST_0),
      Jump(IF_ICMPEQ, Label(29)),
      VarOp(ILOAD, 3),
      IntOp(BIPUSH, 32),
      Op(IOR),
      VarOp(ISTORE, 3),
      Jump(GOTO, Label(29)),
      Label(29),
      VarOp(ALOAD, 0),
      Field(GETFIELD, "SourceMapWriter", "out", "Ljava/io/Writer;"),
      VarOp(ALOAD, 0),
      Invoke(INVOKESPECIAL, "SourceMapWriter", "Base64Map", "()Ljava/lang/String;", itf = false),
      VarOp(ILOAD, 3),
      InvokeVirtual("java/lang/String", "charAt", "(I)C"),
      InvokeVirtual("java/io/Writer", "write", "(I)V"),
      VarOp(ILOAD, 2),
      Op(ICONST_0),
      Jump(IF_ICMPEQ, Label(47)),
      Jump(GOTO, Label(4)),
      Label(47),
      Op(RETURN),
    ))
  }

  @Test def t12835(): Unit = {
    val c1 =
      """def f: Unit = {
        |  val x = 1
        |}
        |""".stripMargin
    val lines = compileMethod(c1).instructions.collect { case l: LineNumber => l }
    assertSameCode(List(LineNumber(2, Label(0))), lines)
  }


  @Test
  def t12990(): Unit = {
    val komp = BytecodeTesting.newCompiler(extraArgs = "-Xasync")
    val code =
      """import scala.tools.nsc.OptionAwait._
        |
        |class C {
        |  def sw1(i: Int) = optionally {
        |    i match {
        |      case 11 if value(Some(430)) > 42 => 22
        |      case p => p
        |    }
        |  }
        |  def sw2(i: Int) = optionally {
        |    i match {
        |      case 11 => if (value(Some(430)) > 42) 22 else i
        |      case p => p
        |    }
        |  }
        |  def sw3(i: Int) = optionally {
        |    i match {
        |      case 11 => if (value(Some(430)) > 42) 22 else i
        |      case 22 | 33 => 44
        |      case p => p
        |    }
        |  }
        |}
        |""".stripMargin
    val cs = komp.compileClasses(code)

    val sm1 = getMethod(cs.find(_.name == "C$stateMachine$async$1").get, "apply")
    assertSame(1, sm1.instructions.count(_.opcode == TABLESWITCH))

    val sm2 = getMethod(cs.find(_.name == "C$stateMachine$async$2").get, "apply")
    assertSame(2, sm2.instructions.count(_.opcode == TABLESWITCH))

    val sm3 = getMethod(cs.find(_.name == "C$stateMachine$async$3").get, "apply")
    assertSame(1, sm3.instructions.count(_.opcode == TABLESWITCH))
    assertSame(1, sm3.instructions.count(_.opcode == LOOKUPSWITCH))
  }
}
