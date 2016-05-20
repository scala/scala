package scala.issues

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.asm.Opcodes
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class BytecodeTest extends BytecodeTesting {
  import compiler._

  @Test
  def t8731(): Unit = {
    val code =
      """class C {
        |  def f(x: Int) = (x: @annotation.switch) match {
        |    case 1 => 0
        |    case 2 => 1
        |    case 3 => 2
        |  }
        |  final val K = 10
        |  def g(x: Int) = (x: @annotation.switch) match {
        |    case K => 0
        |    case 1 => 10
        |    case 2 => 20
        |  }
        |}
      """.stripMargin

    val List(c) = compileClasses(code)

    assertTrue(getSingleMethod(c, "f").instructions.count(_.isInstanceOf[TableSwitch]) == 1)
    assertTrue(getSingleMethod(c, "g").instructions.count(_.isInstanceOf[LookupSwitch]) == 1)
  }

  @Test
  def t8926(): Unit = {
    import scala.reflect.internal.util.BatchSourceFile

    // this test cannot be implemented using partest because of its mixed-mode compilation strategy:
    // partest first compiles all files with scalac, then the java files, and then again the scala
    // using the output classpath. this shadows the bug SI-8926.

    val annotA =
      """import java.lang.annotation.Retention;
        |import java.lang.annotation.RetentionPolicy;
        |@Retention(RetentionPolicy.RUNTIME)
        |public @interface AnnotA { }
      """.stripMargin
    val annotB = "public @interface AnnotB { }"

    val scalaSrc =
      """@AnnotA class A
        |@AnnotB class B
      """.stripMargin

    val run = new global.Run()
    run.compileSources(List(new BatchSourceFile("AnnotA.java", annotA), new BatchSourceFile("AnnotB.java", annotB), new BatchSourceFile("Test.scala", scalaSrc)))
    val outDir = global.settings.outputDirs.getSingleOutput.get
    val outfiles = (for (f <- outDir.iterator if !f.isDirectory) yield (f.name, f.toByteArray)).toList

    def check(classfile: String, annotName: String) = {
      val f = (outfiles collect { case (`classfile`, bytes) => AsmUtils.readClass(bytes) }).head
      val descs = f.visibleAnnotations.asScala.map(_.desc).toList
      assertTrue(descs.toString, descs exists (_ contains annotName))
    }

    check("A.class", "AnnotA")

    // known issue SI-8928: the visibility of AnnotB should be CLASS, but annotation classes without
    // a @Retention annotation are currently emitted as RUNTIME.
    check("B.class", "AnnotB")
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

    val unapplyLineNumbers = getSingleMethod(module, "unapply").instructions.filter(_.isInstanceOf[LineNumber])
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

    val mainIns = getSingleMethod(module, "main").instructions filter {
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

    val List(c) = compileClasses(code)

    // t1: no unnecessary GOTOs
    assertSameCode(getSingleMethod(c, "t1"), List(
      VarOp(ILOAD, 1), Jump(IFEQ, Label(6)),
      Op(ICONST_1), Jump(GOTO, Label(9)),
      Label(6), Op(ICONST_2),
      Label(9), Op(IRETURN)))

    // t2: no unnecessary GOTOs
    assertSameCode(getSingleMethod(c, "t2"), List(
      VarOp(ILOAD, 1), IntOp(SIPUSH, 393), Jump(IF_ICMPNE, Label(7)),
      Op(ICONST_1), Jump(GOTO, Label(10)),
      Label(7), Op(ICONST_2),
      Label(10), Op(IRETURN)))

    // t3: Array == is translated to reference equality, AnyRef == to null checks and equals
    assertSameCode(getSingleMethod(c, "t3"), List(
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
    assertSameCode(getSingleMethod(c, "t4"), t4t5)

    // t5: one side known null, so just a null check on the other
    assertSameCode(getSingleMethod(c, "t5"), t4t5)

    // t6: no unnecessary GOTOs
    assertSameCode(getSingleMethod(c, "t6"), List(
      VarOp(ILOAD, 1), IntOp(BIPUSH, 10), Jump(IF_ICMPNE, Label(7)),
      VarOp(ILOAD, 2), Jump(IFNE, Label(12)),
      Label(7), VarOp(ILOAD, 1), Op(ICONST_1), Jump(IF_ICMPEQ, Label(16)),
      Label(12), Op(ICONST_1), Jump(GOTO, Label(19)),
      Label(16), Op(ICONST_2),
      Label(19), Op(IRETURN)))

    // t7: universal equality
    assertInvoke(getSingleMethod(c, "t7"), "scala/runtime/BoxesRunTime", "equals")

    // t8: no null checks invoking equals on modules and constants
    assertSameCode(getSingleMethod(c, "t8"), List(
      Field(GETSTATIC, "scala/collection/immutable/Nil$", "MODULE$", "Lscala/collection/immutable/Nil$;"), VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false), Jump(IFNE, Label(10)),
      Ldc(LDC, ""), VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false), Jump(IFNE, Label(14)),
      Label(10), Op(ICONST_1), Jump(GOTO, Label(17)),
      Label(14), Op(ICONST_0),
      Label(17), Op(IRETURN)))
  }

  object forwarderTestUtils {
    def findMethods(cls: ClassNode, name: String): List[Method] = cls.methods.iterator.asScala.find(_.name == name).map(convertMethod).toList

    import language.implicitConversions
    implicit def s2c(s: Symbol)(implicit classes: Map[String, ClassNode]): ClassNode = classes(s.name)

    def checkForwarder(c: ClassNode, target: String) = {
      val List(f) = findMethods(c, "f")
      assertSameCode(f, List(VarOp(ALOAD, 0), Invoke(INVOKESPECIAL, target, "f", "()I", false), Op(IRETURN)))
    }
  }

  @Test
  def traitMethodForwarders(): Unit = {
    import forwarderTestUtils._
    val code =
      """trait T1 { def f = 1 }
        |trait T2 extends T1 { override def f = 2 }
        |trait T3 { self: T1 => override def f = 3 }
        |
        |abstract class A1 { def f: Int }
        |class A2 { def f: Int = 4 }
        |
        |trait T4 extends A1 { def f = 5 }
        |trait T5 extends A2 { override def f = 6 }
        |
        |trait T6 { def f: Int }
        |trait T7 extends T6 { abstract override def f = super.f + 1 }
        |
        |trait T8 { override def clone() = super.clone() }
        |
        |class A3 extends T1 { override def f = 7 }
        |
        |class C1 extends T1
        |class C2 extends T2
        |class C3 extends T1 with T2
        |class C4 extends T2 with T1
        |class C5 extends T1 with T3
        |
        |// traits extending a class that defines f
        |class C6 extends T4
        |class C7 extends T5
        |class C8 extends A1 with T4
        |class C9 extends A2 with T5
        |
        |// T6: abstract f in trait
        |class C10 extends T6 with T1
        |class C11 extends T6 with T2
        |abstract class C12 extends A1 with T6
        |class C13 extends A2 with T6
        |class C14 extends T4 with T6
        |class C15 extends T5 with T6
        |
        |// superclass overrides a trait method
        |class C16 extends A3
        |class C17 extends A3 with T1
        |
        |// abstract override
        |class C18 extends T6 { def f = 22 }
        |class C19 extends C18 with T7
        |
        |class C20 extends T8
      """.stripMargin

    implicit val classes = compileClasses(code).map(c => (c.name, c)).toMap

    val noForwarder = List('C1, 'C2, 'C3, 'C4, 'C10, 'C11, 'C12, 'C13, 'C16, 'C17)
    for (c <- noForwarder) assertEquals(findMethods(c, "f"), Nil)

    checkForwarder('C5, "T3")
    checkForwarder('C6, "T4")
    checkForwarder('C7, "T5")
    checkForwarder('C8, "T4")
    checkForwarder('C9, "T5")
    checkForwarder('C14, "T4")
    checkForwarder('C15, "T5")
    assertSameSummary(getSingleMethod('C18, "f"), List(BIPUSH, IRETURN))
    checkForwarder('C19, "T7")
    assertSameCode(getSingleMethod('C19, "T7$$super$f"), List(VarOp(ALOAD, 0), Invoke(INVOKESPECIAL, "C18", "f", "()I", false), Op(IRETURN)))
    assertInvoke(getSingleMethod('C20, "clone"), "T8", "clone") // mixin forwarder
  }

  @Test
  def noTraitMethodForwardersForOverloads(): Unit = {
    import forwarderTestUtils._
    val code =
      """trait T1 { def f(x: Int) = 0 }
        |trait T2 { def f(x: String) = 1 }
        |class C extends T1 with T2
      """.stripMargin
    val List(c, t1, t2) = compileClasses(code)
    assertEquals(findMethods(c, "f"), Nil)
  }

  @Test
  def traitMethodForwardersForJavaDefaultMethods(): Unit = {
    import forwarderTestUtils._
    val j1 = ("interface J1 { int f(); }", "J1.java")
    val j2 = ("interface J2 { default int f() { return 1; } }", "J2.java")
    val j3 = ("interface J3 extends J1 { default int f() { return 2; } }", "J3.java")
    val j4 = ("interface J4 extends J2 { default int f() { return 3; } }", "J4.java")
    val code =
      """trait T1 extends J2 { override def f = 4 }
        |trait T2 { self: J2 => override def f = 5 }
        |
        |class K1 extends J2
        |class K2 extends J1 with J2
        |class K3 extends J2 with J1
        |
        |class K4 extends J3
        |class K5 extends J3 with J1
        |class K6 extends J1 with J3
        |
        |class K7 extends J4
        |class K8 extends J4 with J2
        |class K9 extends J2 with J4
        |
        |class K10 extends T1 with J2
        |class K11 extends J2 with T1
        |
        |class K12 extends J2 with T2
      """.stripMargin
    implicit val classes = compileClasses(code, List(j1, j2, j3, j4)).map(c => (c.name, c)).toMap

    val noForwarder = List('K1, 'K2, 'K3, 'K4, 'K5, 'K6, 'K7, 'K8, 'K9, 'K10, 'K11)
    for (c <- noForwarder) assertEquals(findMethods(c, "f"), Nil)

    checkForwarder('K12, "T2")
  }

  @Test
  def invocationReceivers(): Unit = {
    val List(c1, c2, t, u) = compileClasses(invocationReceiversTestCode.definitions("Object"))
    // mixin forwarder in C1
    assertSameCode(getSingleMethod(c1, "clone"), List(VarOp(ALOAD, 0), Invoke(INVOKESPECIAL, "T", "clone", "()Ljava/lang/Object;", false), Op(ARETURN)))
    assertInvoke(getSingleMethod(c1, "f1"), "T", "clone")
    assertInvoke(getSingleMethod(c1, "f2"), "T", "clone")
    assertInvoke(getSingleMethod(c1, "f3"), "C1", "clone")
    assertInvoke(getSingleMethod(c2, "f1"), "T", "clone")
    assertInvoke(getSingleMethod(c2, "f2"), "T", "clone")
    assertInvoke(getSingleMethod(c2, "f3"), "C1", "clone")

    val List(c1b, c2b, tb, ub) = compileClasses(invocationReceiversTestCode.definitions("String"))
    def ms(c: ClassNode, n: String) = c.methods.asScala.toList.filter(_.name == n)
    assert(ms(tb, "clone").length == 1)
    assert(ms(ub, "clone").isEmpty)
    val List(c1Clone) = ms(c1b, "clone")
    assertEquals(c1Clone.desc, "()Ljava/lang/Object;")
    assert((c1Clone.access | Opcodes.ACC_BRIDGE) != 0)
    assertSameCode(convertMethod(c1Clone), List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C1", "clone", "()Ljava/lang/String;", false), Op(ARETURN)))

    def iv(m: Method) = getSingleMethod(c1b, "f1").instructions.collect({case i: Invoke => i})
    assertSameCode(iv(getSingleMethod(c1b, "f1")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
    assertSameCode(iv(getSingleMethod(c1b, "f2")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
    // invokeinterface T.clone in C1 is OK here because it is not an override of Object.clone (different siganture)
    assertSameCode(iv(getSingleMethod(c1b, "f3")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
  }

  @Test
  def invocationReceiversProtected(): Unit = {
    // http://lrytz.github.io/scala-aladdin-bugtracker/displayItem.do%3Fid=455.html / 9954eaf
    // also https://issues.scala-lang.org/browse/SI-1430 / 0bea2ab (same but with interfaces)
    val aC =
      """package a;
        |/*package private*/ abstract class A {
        |  public int f() { return 1; }
        |  public int t;
        |}
      """.stripMargin
    val bC =
      """package a;
        |public class B extends A { }
      """.stripMargin
    val iC =
      """package a;
        |/*package private*/ interface I { int f(); }
      """.stripMargin
    val jC =
      """package a;
        |public interface J extends I { }
      """.stripMargin
    val cC =
      """package b
        |class C {
        |  def f1(b: a.B) = b.f
        |  def f2(b: a.B) = { b.t = b.t + 1 }
        |  def f3(j: a.J) = j.f
        |}
      """.stripMargin
    val List(c) = compileClasses(cC, javaCode = List((aC, "A.java"), (bC, "B.java"), (iC, "I.java"), (jC, "J.java")))
    assertInvoke(getSingleMethod(c, "f1"), "a/B", "f") // receiver needs to be B (A is not accessible in class C, package b)
    assertInvoke(getSingleMethod(c, "f3"), "a/J", "f") // receiver needs to be J
  }

  @Test
  def specialInvocationReceivers(): Unit = {
    val code =
      """class C {
        |  def f1(a: Array[String]) = a.clone()
        |  def f2(a: Array[Int]) = a.hashCode()
        |  def f3(n: Nothing) = n.hashCode()
        |  def f4(n: Null) = n.toString()
        |
        |}
      """.stripMargin
    val List(c) = compileClasses(code)
    assertInvoke(getSingleMethod(c, "f1"), "[Ljava/lang/String;", "clone") // array descriptor as receiver
    assertInvoke(getSingleMethod(c, "f2"), "java/lang/Object", "hashCode") // object receiver
    assertInvoke(getSingleMethod(c, "f3"), "java/lang/Object", "hashCode")
    assertInvoke(getSingleMethod(c, "f4"), "java/lang/Object", "toString")
  }

  @Test
  def superConstructorArgumentInSpecializedClass(): Unit = {
    // see comment in SpecializeTypes.forwardCtorCall
    val code = "case class C[@specialized(Int) T](_1: T)"
    val List(c, cMod, cSpec) = compileClasses(code)
    assertSameSummary(getSingleMethod(cSpec, "<init>"),
      // pass `null` to super constructor, no box-unbox, no Integer created
      List(ALOAD, ILOAD, PUTFIELD, ALOAD, ACONST_NULL, "<init>", RETURN))
  }
}

object invocationReceiversTestCode {
  // if cloneType is more specific than Object (e.g., String), a bridge method is generated.
  def definitions(cloneType: String) =
   s"""trait T { override def clone(): $cloneType = "hi" }
      |trait U extends T
      |class C1 extends U with Cloneable {
      |  // The comments below are true when $cloneType is Object.
      |  // C1 gets a forwarder for clone that invokes T.clone. this is needed because JVM method
      |  // resolution always prefers class members, so it would resolve to Object.clone, even if
      |  // C1 is a subtype of the interface T which has an overriding default method for clone.
      |
      |  // invokeinterface T.clone
      |  def f1 = (this: T).clone()
      |
      |  // cannot invokeinterface U.clone (NoSuchMethodError). Object.clone would work here, but
      |  // not in the example in C2 (illegal access to protected). T.clone works in all cases and
      |  // resolves correctly.
      |  def f2 = (this: U).clone()
      |
      |  // invokevirtual C1.clone()
      |  def f3 = (this: C1).clone()
      |}
      |
      |class C2 {
      |  def f1(t: T) = t.clone()  // invokeinterface T.clone
      |  def f2(t: U) = t.clone()  // invokeinterface T.clone -- Object.clone would be illegal (protected, explained in C1)
      |  def f3(t: C1) = t.clone() // invokevirtual C1.clone -- Object.clone would be illegal
      |}
    """.stripMargin

  val runCode =
    """
      |val r = new StringBuffer()
      |val c1 = new C1
      |r.append(c1.f1)
      |r.append(c1.f2)
      |r.append(c1.f3)
      |val t = new T { }
      |val u = new U { }
      |val c2 = new C2
      |r.append(c2.f1(t))
      |r.append(c2.f1(u))
      |r.append(c2.f1(c1))
      |r.append(c2.f2(u))
      |r.append(c2.f2(c1))
      |r.append(c2.f3(c1))
      |r.toString
    """.stripMargin
}
