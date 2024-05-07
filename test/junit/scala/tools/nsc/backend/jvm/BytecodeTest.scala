package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._
import scala.collection.JavaConverters._
import scala.tools.asm.Opcodes

@RunWith(classOf[JUnit4])
class BytecodeTest extends BytecodeTesting {
  import compiler._

  @Test
  def staticForwardersBridgeFlag(): Unit = {
    val code =
      """ A {
        |  def f: Object = null
        |  def g: Object
        |}
        |object B extends A {
        |  override def f: String = "b"
        |  def g: String = "b"
        |}
        |case class K(x: Int, s: String)
      """.stripMargin
    for (base <- List("trait", "abstract class")) {
      val List(a, bMirror, bModule, kClass, kModule) = compileClasses(base + code)
      assertEquals("B", bMirror.name)
      assertEquals(List("f()Ljava/lang/String;0x9", "g()Ljava/lang/String;0x9"),
        bMirror.methods.asScala
          .filter(m => m.name == "f" || m.name == "g")
          .map(m => m.name + m.desc + "0x" + Integer.toHexString(m.access)).toList.sorted)
      assertEquals("K", kClass.name)
      val List(app) = kClass.methods.asScala.filter(_.name == "apply").toList
      assertEquals("apply(ILjava/lang/String;)LK;0x9", app.name + app.desc + "0x" + Integer.toHexString(app.access))
    }
  }

  @Test
  def staticForwardersVarargFlag(): Unit = {
    val code =
      """ A { @annotation.varargs def f(i: Int*): Object = null }
        |object B extends A { @annotation.varargs override def f(i: Int*): String = "b" }
      """.stripMargin
    for (base <- List("trait", "class")) {
      val List(a, bMirror, bModule) = compileClasses(base + code)
      assertEquals("B", bMirror.name)
      assertEquals(List(
        "f(Lscala/collection/Seq;)Ljava/lang/String;0x9",
        "f([I)Ljava/lang/String;0x89"),
        bMirror.methods.asScala
          .filter(_.name == "f")
          .map(m => m.name + m.desc + "0x" + Integer.toHexString(m.access)).toList.sorted)
    }
  }

  @Test
  def t6288bJumpPosition(): Unit = {
    val code =
      """object Case3 {                                 // 01
        | def unapply(z: Any): Option[Int] = Some(-1)   // 02
        | def main(args: Array[String]) {               // 03
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
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
      Jump(GOTO, Label(33)),

      LineNumber(5, Label(20)),
      Jump(GOTO, Label(24)),

      LineNumber(8, Label(24)),
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
      Jump(GOTO, Label(33)),

      LineNumber(10, Label(33)),
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false)
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
        |  def t8(a: AnyRef) = Nil == a || "" != a
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
      Label(14), VarOp(ALOAD, 3), Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false), Jump(IFEQ, Label(23)),
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
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"), VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false), Jump(IFNE, Label(10)),
      Ldc(LDC, ""), VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false), Jump(IFNE, Label(14)),
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
      Label(9), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "foo", "()V", false), Label(13), Op(ACONST_NULL), VarOp(ASTORE, 1), Label(17), Jump(GOTO, Label(4)),
      Label(20), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "bar", "()V", false), Op(RETURN), Label(26)))
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
    val invoke = Invoke(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;", false)
    // ths static accessor is positioned at the line number of the accessed method.
    assertSameCode(tMethod.instructions,
      List(Label(0), LineNumber(2, Label(0)), VarOp(ALOAD, 0), Invoke(INVOKESPECIAL, "T", "t", "()V", true), Op(RETURN), Label(4))
    )
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
        List("<clinit>", "$lessinit$greater$default$1", "toString", "apply", "apply$default$1", "unapply", "readResolve", "apply", "<init>"))
    }
    check(s"$main\n$person")
    check(s"$person\n$main")
  }

  @Test
  def t11412(): Unit = {
    val code = "class A { val a = 0 }; class C extends A with App { val x = 1; val y = x }"
    val cs = compileClasses(code)
    val c = cs.find(_.name == "C").get
    val fs = c.fields.asScala.toList.sortBy(_.name).map(f => (f.name, (f.access & Opcodes.ACC_FINAL) != 0))
    assertEquals(List(
      ("executionStart", true), // final in 2.12.x, but that's problem with mixin. was fixed in 2.13 (https://github.com/scala/scala/pull/7028)
      ("scala$App$$_args", false),
      ("scala$App$$initCode", true), // also a mixin
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
      ("$outer", true), // mixin
      ("executionStart", true),
      ("scala$App$$_args", false), // mixin
      ("scala$App$$initCode", true),
      ("x$1", true), // captured, assigned in constructor
      ("y$1", true)  // captured
    ), fs)
    val assignedInConstr = getMethod(k, "<init>").instructions.collect {
      case f: Field if f.opcode == Opcodes.PUTFIELD => f.name
    }
    assertEquals(List("$outer", "x$1", "y$1"), assignedInConstr.sorted)
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
  def sortedSetMapEqualsSuperAccessor(): Unit = {
    // ensure super accessors are there (scala/scala#9311)

    val sn = "scala$collection$immutable$SortedSet$$super$equals"
    val sm = classOf[scala.collection.immutable.TreeSet[_]].getDeclaredMethod(sn, classOf[Object])
    assertEquals(sn, sm.getName)

    val mn = "scala$collection$immutable$SortedMap$$super$equals"
    val mm = classOf[scala.collection.immutable.TreeMap[_, _]].getDeclaredMethod(mn, classOf[Object])
    assertEquals(mn, mm.getName)
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
      Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;", false),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false),
      VarOp(ISTORE, 5),
      VarOp(ILOAD, 5),
      Op(IRETURN),
      Label(19),
      Jump(GOTO, Label(22)),
      Label(22),
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"),
      VarOp(ALOAD, 3),
      Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false),
      Jump(IFEQ, Label(31)),
      IntOp(BIPUSH, 20),
      Op(IRETURN),
      Label(31),
      Jump(GOTO, Label(34)),
      Label(34),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ALOAD, 3),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", false),
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
      Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;", false),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false),
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
      Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;", false),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false),
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
      Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;", false),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false),
      VarOp(ISTORE, 5),
      Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
      VarOp(ILOAD, 5),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", false),
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
      Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"),
      Op(POP),
      Op(RETURN),
      Label(24),
      Jump(GOTO, Label(27)),
      Label(27),
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"),
      VarOp(ALOAD, 3),
      Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false),
      Jump(IFEQ, Label(40)),
      Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
      Ldc(LDC, "nil"),
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
      Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"),
      Op(POP),
      Op(RETURN),
      Label(40),
      Jump(GOTO, Label(43)),
      Label(43),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ALOAD, 3),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", false),
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
      Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;", false),
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false),
      VarOp(ISTORE, 5),
      VarOp(ILOAD, 5),
      Op(IRETURN),
      Label(19),
      Jump(GOTO, Label(22)),
      Label(22),
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"),
      VarOp(ALOAD, 3),
      Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false),
      Jump(IFEQ, Label(31)),
      IntOp(BIPUSH, 20),
      Op(IRETURN),
      Label(31),
      Jump(GOTO, Label(34)),
      Label(34),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ALOAD, 3),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", false),
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
      Invoke(INVOKESPECIAL, "A", "<init>", "()V", false),
      Jump(GOTO, Label(41)),
      Label(16),
      Jump(GOTO, Label(19)),
      Label(19),
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"),
      VarOp(ALOAD, 4),
      Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false),
      Jump(IFEQ, Label(30)),
      TypeOp(NEW, "B"),
      Op(DUP),
      Invoke(INVOKESPECIAL, "B", "<init>", "()V", false),
      Jump(GOTO, Label(41)),
      Label(30),
      Jump(GOTO, Label(33)),
      Label(33),
      TypeOp(NEW, "scala/MatchError"),
      Op(DUP),
      VarOp(ALOAD, 4),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", false),
      Op(ATHROW),
      Label(41),
      TypeOp(CHECKCAST, "Tree"),
      Invoke(INVOKEVIRTUAL, "PatmatAdaptMatchEnd", "atPos", "(LTree;)LTree;", false),
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
      Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", false),
      Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", false),
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
      Invoke(INVOKEVIRTUAL, "java/io/Writer", "write", "(I)V", false),
      Op(RETURN),
      Label(34),
      VarOp(ALOAD, 0),
      VarOp(ILOAD, 3),
      Invoke(INVOKESPECIAL, "SourceMapWriter", "writeBase64VLQSlowPath$1", "(I)V", false),
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
      Invoke(INVOKESPECIAL, "SourceMapWriter", "Base64Map", "()Ljava/lang/String;", false),
      VarOp(ILOAD, 3),
      Invoke(INVOKEVIRTUAL, "java/lang/String", "charAt", "(I)C", false),
      Invoke(INVOKEVIRTUAL, "java/io/Writer", "write", "(I)V", false),
      VarOp(ILOAD, 2),
      Op(ICONST_0),
      Jump(IF_ICMPEQ, Label(47)),
      Jump(GOTO, Label(4)),
      Label(47),
      Op(RETURN),
    ))
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
