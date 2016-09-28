package scala.lang.traits

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.asm.Opcodes
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree.ClassNode
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class BytecodeTest extends BytecodeTesting {
  import compiler._

  val noForwardersCompiler = newCompiler(extraArgs = "-Xmixin-force-forwarders:false")

  def checkForwarder(classes: Map[String, ClassNode], clsName: Symbol, target: String) = {
    val f = getMethod(classes(clsName.name), "f")
    assertSameCode(f, List(VarOp(ALOAD, 0), Invoke(INVOKESTATIC, target, "f$", s"(L$target;)I", true), Op(IRETURN)))
  }

  @Test
  def traitMethodForwarders(): Unit = {
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

    val c = noForwardersCompiler.compileClasses(code).map(c => (c.name, c)).toMap

    val noForwarder = List('C1, 'C2, 'C3, 'C4, 'C10, 'C11, 'C12, 'C13, 'C16, 'C17)
    for (cn <- noForwarder) assertEquals(getMethods(c(cn.name), "f"), Nil)

    checkForwarder(c, 'C5, "T3")
    checkForwarder(c, 'C6, "T4")
    checkForwarder(c, 'C7, "T5")
    checkForwarder(c, 'C8, "T4")
    checkForwarder(c, 'C9, "T5")
    checkForwarder(c, 'C14, "T4")
    checkForwarder(c, 'C15, "T5")
    assertSameSummary(getMethod(c("C18"), "f"), List(BIPUSH, IRETURN))
    checkForwarder(c, 'C19, "T7")
    assertSameCode(getMethod(c("C19"), "T7$$super$f"), List(VarOp(ALOAD, 0), Invoke(INVOKESPECIAL, "C18", "f", "()I", false), Op(IRETURN)))
    assertInvoke(getMethod(c("C20"), "clone"), "T8", "clone$") // mixin forwarder
  }

  @Test
  def noTraitMethodForwardersForOverloads(): Unit = {
    val code =
      """trait T1 { def f(x: Int) = 0 }
        |trait T2 { def f(x: String) = 1 }
        |class C extends T1 with T2
      """.stripMargin
    val List(c, t1, t2) = noForwardersCompiler.compileClasses(code)
    assertEquals(getMethods(c, "f"), Nil)
  }

  @Test
  def traitMethodForwardersForJavaDefaultMethods(): Unit = {
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
    val c = noForwardersCompiler.compileClasses(code, List(j1, j2, j3, j4)).map(c => (c.name, c)).toMap

    val noForwarder = List('K1, 'K2, 'K3, 'K4, 'K5, 'K6, 'K7, 'K8, 'K9, 'K10, 'K11)
    for (cn <- noForwarder) assertEquals(getMethods(c(cn.name), "f"), Nil)

    checkForwarder(c, 'K12, "T2")
  }

  @Test
  def invocationReceivers(): Unit = {
    val List(c1, c2, t, u) = noForwardersCompiler.compileClasses(invocationReceiversTestCode.definitions("Object"))
    // mixin forwarder in C1
    assertSameCode(getMethod(c1, "clone"), List(VarOp(ALOAD, 0), Invoke(INVOKESTATIC, "T", "clone$", "(LT;)Ljava/lang/Object;", true), Op(ARETURN)))
    assertInvoke(getMethod(c1, "f1"), "T", "clone")
    assertInvoke(getMethod(c1, "f2"), "T", "clone")
    assertInvoke(getMethod(c1, "f3"), "C1", "clone")
    assertInvoke(getMethod(c2, "f1"), "T", "clone")
    assertInvoke(getMethod(c2, "f2"), "T", "clone")
    assertInvoke(getMethod(c2, "f3"), "C1", "clone")

    val List(c1b, c2b, tb, ub) = noForwardersCompiler.compileClasses(invocationReceiversTestCode.definitions("String"))
    def ms(c: ClassNode, n: String) = c.methods.asScala.toList.filter(_.name == n)
    assert(ms(tb, "clone").length == 1)
    assert(ms(ub, "clone").isEmpty)
    val List(c1Clone) = ms(c1b, "clone")
    assertEquals(c1Clone.desc, "()Ljava/lang/Object;")
    assert((c1Clone.access | Opcodes.ACC_BRIDGE) != 0)
    assertSameCode(convertMethod(c1Clone), List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C1", "clone", "()Ljava/lang/String;", false), Op(ARETURN)))

    def iv(m: Method) = getInstructions(c1b, "f1").collect({case i: Invoke => i})
    assertSameCode(iv(getMethod(c1b, "f1")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
    assertSameCode(iv(getMethod(c1b, "f2")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
    // invokeinterface T.clone in C1 is OK here because it is not an override of Object.clone (different siganture)
    assertSameCode(iv(getMethod(c1b, "f3")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
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
    val c = compileClass(cC, javaCode = List((aC, "A.java"), (bC, "B.java"), (iC, "I.java"), (jC, "J.java")))
    assertInvoke(getMethod(c, "f1"), "a/B", "f") // receiver needs to be B (A is not accessible in class C, package b)
    assertInvoke(getMethod(c, "f3"), "a/J", "f") // receiver needs to be J
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
    val c = compileClass(code)
    assertInvoke(getMethod(c, "f1"), "[Ljava/lang/String;", "clone") // array descriptor as receiver
    assertInvoke(getMethod(c, "f2"), "java/lang/Object", "hashCode") // object receiver
    assertInvoke(getMethod(c, "f3"), "java/lang/Object", "hashCode")
    assertInvoke(getMethod(c, "f4"), "java/lang/Object", "toString")
  }

  @Test
  def superConstructorArgumentInSpecializedClass(): Unit = {
    // see comment in SpecializeTypes.forwardCtorCall
    val code = "case class C[@specialized(Int) T](_1: T)"
    val List(c, cMod, cSpec) = compileClasses(code)
    assertSameSummary(getMethod(cSpec, "<init>"),
      // pass `null` to super constructor, no box-unbox, no Integer created
      List(ALOAD, ILOAD, PUTFIELD, ALOAD, ACONST_NULL, "<init>", RETURN))
  }

  @Test
  def mixinForwarders(): Unit = {
    val code =
      """trait T { def f = 1 }
        |class C extends T
      """.stripMargin
    val List(c1, _) = noForwardersCompiler.compileClasses(code)
    val List(c2, _) = compileClasses(code)
    assert(getMethods(c1, "f").isEmpty)
    assertSameCode(getMethod(c2, "f"),
      List(VarOp(ALOAD, 0), Invoke(INVOKESTATIC, "T", "f$", "(LT;)I", true), Op(IRETURN)))
  }

  @Test
  def sd143(): Unit = {
    val code =
      """class A { def m = 1 }
        |class B extends A { override def m = 2 }
        |trait T extends A
        |class C extends B with T {
        |  override def m = super[T].m // should invoke A.m
        |}
      """.stripMargin

    val err =
      """cannot emit super call: the selected method m is declared in class A, which is not the direct superclass of class C.
        |An unqualified super call (super.m) would be allowed.""".stripMargin
    val cls = compileClasses(code, allowMessage = _.msg contains err)
    assert(cls.isEmpty, cls.map(_.name))
  }

  @Test
  def sd143b(): Unit = {
    val jCode = List("interface A { default int m() { return 1; } }" -> "A.java")
    val code =
      """class B extends A { override def m = 2 }
        |trait T extends A
        |class C extends B with T {
        |  override def m = super[T].m
        |}
      """.stripMargin

    val err = "unable to emit super call unless interface A (which declares method m) is directly extended by class C"
    val cls = compileClasses(code, jCode, allowMessage = _.msg contains err)
    assert(cls.isEmpty, cls.map(_.name))
  }

  @Test
  def sd143c(): Unit = {
    // Allow super calls to class methods of indirect super classes
    val code =
      """class A { def f = 1 }
        |class B extends A
        |trait T extends A { override def f = 2 }
        |class C extends B with T {
        |  def t1 = super[B].f
        |  def t2 = super.f
        |  def t3 = super[T].f
        |}
      """.stripMargin
    val List(_, _, c, _) = compileClasses(code)
    val t1 = getInstructions(c, "t1")
    assert(t1 contains Invoke(INVOKESPECIAL, "A", "f", "()I", false), t1.stringLines)
    val t2 = getInstructions(c, "t2")
    val invStat = Invoke(INVOKESTATIC, "T", "f$", "(LT;)I", true)
    assert(t2 contains invStat, t2.stringLines)
    val t3 = getInstructions(c, "t3")
    assert(t3 contains invStat, t3.stringLines)
  }

  @Test
  def sd210(): Unit = {
    val jCode = List("interface A { default int m() { return 1; } }" -> "A.java")


    // used to crash in the backend (SD-210) under `-Xmixin-force-forwarders:true`
    val code1 =
      """trait B1 extends A // called "B1" not "B" due to scala-dev#214
        |class C extends B1
      """.stripMargin

    val List(_, c1a) = noForwardersCompiler.compileClasses(code1, jCode)
    assert(getAsmMethods(c1a, "m").isEmpty) // ok, no forwarder

    // here we test a warning. without `-Xmixin-force-forwarders:true`, the forwarder would not be
    // generated, it is not necessary for correctness.
    val List(_, c1b) = compileClasses(code1, jCode)
    assert(getAsmMethods(c1b, "m").isEmpty) // no forwarder: it cannot be implemented because A is not a direct parent of C


    val code2 =
      """abstract class B { def m(): Int }
        |trait T extends B with A
        |class C extends T
      """.stripMargin

    // here we test a compilation error. the forwarder is required for correctness, but it cannot be generated.
    val err = "Unable to implement a mixin forwarder for method m in class C unless interface A is directly extended by class C"
    val cs = compileClasses(code2, jCode, allowMessage = _.msg contains err)
    assert(cs.isEmpty, cs.map(_.name))


    val code3 =
      """abstract class B { def m: Int }
        |class C extends B with A
      """.stripMargin

    val List(_, c3) = compileClasses(code3, jCode)
    // invokespecial to A.m is correct here: A is an interface, so resolution starts at A.
    // https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.invokespecial
    val ins3 = getMethod(c3, "m").instructions
    assert(ins3 contains Invoke(INVOKESPECIAL, "A", "m", "()I", true), ins3.stringLines)


    val code4 =
      """trait B { self: A => override def m = 2 }
        |class C extends A with B // forwarder, invokestatic B.m$
      """.stripMargin

    val List(_, c4) = compileClasses(code4, jCode)
    val ins4 = getMethod(c4, "m").instructions
    assert(ins4 contains Invoke(INVOKESTATIC, "B", "m$", "(LB;)I", true), ins4.stringLines)


    // scala-only example
    val code5 =
      """trait AS { def m = 1 }
        |abstract class B { def m: Int }
        |class C extends B with AS // forwarder, invokestatic AS.m$
      """.stripMargin

    val List(_, _, c5) = compileClasses(code5)
    val ins5 = getMethod(c5, "m").instructions
    assert(ins5 contains Invoke(INVOKESTATIC, "AS", "m$", "(LAS;)I", true), ins5.stringLines)
  }

  @Test
  def sd224(): Unit = {
    val jCode = List("interface T { default int f() { return 1; } }" -> "T.java")
    val code =
      """trait U extends T
        |class C extends U { def t = super.f }
      """.stripMargin
    val msg = "unable to emit super call unless interface T (which declares method f) is directly extended by class C"
    val cls = compileClasses(code, jCode, allowMessage = _.msg contains msg)
    assertEquals(cls, Nil)
  }

  def ifs(c: ClassNode, expected: List[String]) = assertEquals(expected, c.interfaces.asScala.toList.sorted)
  def invSt(m: Method, receiver: String, method: String = "f$", itf: Boolean = true): Unit =
    assert(m.instructions contains Invoke(INVOKESTATIC, receiver, method, s"(L$receiver;)I", itf), m.instructions.stringLines)
  def invSp(m: Method, receiver: String, method: String = "f", sig: String = "()I", itf: Boolean = true): Unit =
    assert(m.instructions contains Invoke(INVOKESPECIAL, receiver, method, sig, itf), m.instructions.stringLines)

  @Test
  def superCalls1(): Unit = {
    val code =
      """trait T { def f = 1 }
        |trait U extends T
        |class C extends U { def t = super.f }
      """.stripMargin
    val List(c, _*) = compileClasses(code)
    ifs(c, List("U"))
    invSt(getMethod(c, "t"), "T")
    invSt(getMethod(c, "f"), "T")
  }

  @Test
  def superCalls2(): Unit = {
    val code =
      """class A { def f = 1 }
        |trait T extends A { override def f = 2 }
        |class B extends A
        |class C extends B with T {
        |  def t1 = super.f
        |  def t2 = super[T].f
        |  def t3 = super[B].f
        |}
      """.stripMargin
    val List(_, _, c, _) = compileClasses(code)
    invSt(getMethod(c, "f"), "T")
    invSt(getMethod(c, "t1"), "T")
    invSt(getMethod(c, "t2"), "T")
    invSp(getMethod(c, "t3"), "A", itf = false)
  }

  @Test
  def superCalls3(): Unit = {
    val code =
      """class A { def f = 1 }
        |trait T extends A
        |class B extends A { override def f = 2 }
        |class C extends B with T {
        |  def t1 = super.f
        |  // def t2 = super[T].f // error: cannot emit super call. tested in sd143
        |  def t3 = super[B].f
        |}
      """.stripMargin
    val List(_, _, c, _) = compileClasses(code)
    invSp(getMethod(c, "t1"), "B", itf = false)
    invSp(getMethod(c, "t3"), "B", itf = false)
    assertEquals(getMethods(c, "f"), Nil)
  }

  @Test
  def superCalls4(): Unit = {
    val code =
      """trait T1 { def f = 1 }
        |trait T2 { self: T1 => override def f = 2 }
        |trait U extends T1 with T2
        |class C extends U {
        |  def t1 = super.f
        |  def t2 = super[U].f
        |}
      """.stripMargin
    val List(c, _*) = compileClasses(code)
    ifs(c, List("U"))
    invSt(getMethod(c, "f"), "T2")
    invSt(getMethod(c, "t1"), "T2")
    invSt(getMethod(c, "t2"), "T2")
  }

  @Test
  def superCalls5(): Unit = {
    val code =
      """trait T1 { def f = 1 }
        |trait T2 { self: T1 => override def f = 2 }
        |trait U extends T1 with T2
        |class C extends U with T1 with T2
      """.stripMargin
    val List(c, _*) = compileClasses(code)
    ifs(c, List("U")) // T1, T2 removed by minimizeParents
    invSt(getMethod(c, "f"), "T2")
  }

  @Test
  def superCalls6(): Unit = {
    val code =
      """trait T { override def hashCode = -1 }
        |trait U extends T
        |class C extends U {
        |  def t1 = super[U].hashCode
        |  def t2 = super.hashCode
        |}
      """.stripMargin
    val List(c, _*) = compileClasses(code)
    ifs(c, List("U"))
    invSt(getMethod(c, "hashCode"), "T", "hashCode$")
    invSt(getMethod(c, "t1"), "T", "hashCode$")
    invSt(getMethod(c, "t2"), "T", "hashCode$")
  }

  @Test
  def superCalls7(): Unit = {
    val code =
      """trait T { def f = 1 }
        |trait U1 extends T { override def f = 2 }
        |trait U2 extends T { override def f = 3 }
        |class C1 extends T with U1 with U2 {
        |  def t1 = super.f
        |  def t2 = super[T].f
        |  def t3 = super[U1].f
        |  def t4 = super[U2].f
        |}
        |class C2 extends T with U2 with U1 {
        |  def t1 = super.f
        |}
      """.stripMargin
    val List(c1, c2, _*) = compileClasses(code)
    ifs(c1, List("U1", "U2"))
    ifs(c2, List("U1", "U2"))
    invSt(getMethod(c1, "f"), "U2")
    invSt(getMethod(c1, "t1"), "U2")
    invSt(getMethod(c1, "t2"), "T")
    invSt(getMethod(c1, "t3"), "U1")
    invSt(getMethod(c1, "t4"), "U2")
    invSt(getMethod(c2, "f"), "U1")
    invSt(getMethod(c2, "t1"), "U1")
  }

  @Test
  def superCalls8(): Unit = {
    val code =
      """trait T1 { def f = 1 }
        |trait T2 { _: T1 => override def f = 2 }
        |trait U extends T1 with T2
        |trait V extends U with T2
        |class C extends V {
        |  def t1 = super.f
        |  def t2 = super[V].f
        |}
      """.stripMargin
    val List(c, _*) = compileClasses(code)
    ifs(c, List("V"))
    invSt(getMethod(c, "f"), "T2")
    invSt(getMethod(c, "t1"), "T2")
    invSt(getMethod(c, "t2"), "T2")
  }

  @Test
  def superCalls9(): Unit = {
    val code =
      """trait T { def f: Int }
        |trait U1 extends T { def f = 0 }
        |trait U2 extends T { override def f = 1 }
        |trait V extends U1
        |
        |trait W1 extends V with U2
        |class C1 extends W1 with U2
        |
        |trait W2 extends V with U2 { override def f = super[U2].f }
        |class C2 extends W2 with U2
        |
        |trait W3 extends V with U2 { override def f = super.f }
        |class C3 extends W3 with U2
      """.stripMargin
    val List(c1, c2, c3, _*) = compileClasses(code)

    ifs(c1, List("W1"))
    invSt(getMethod(c1, "f"), "U2")

    ifs(c2, List("W2"))
    invSt(getMethod(c2, "f"), "W2")

    ifs(c3, List("W3"))
    invSt(getMethod(c3, "W3$$super$f"), "U2")
    invSt(getMethod(c3, "f"), "W3")
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
