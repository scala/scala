import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.{ClassNode, InnerClassNode}
import asm.{Opcodes => Flags}
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def assertSame(a: Any, b: Any) = {
    assert(a == b, s"\na: $a\nb: $b")
  }

  val publicStatic = Flags.ACC_PUBLIC | Flags.ACC_STATIC 
  val publicAbstractInterface = Flags.ACC_PUBLIC | Flags.ACC_ABSTRACT | Flags.ACC_INTERFACE

  def innerClassNodes(className: String): List[InnerClassNode] = {
    loadClassNode(className).innerClasses.asScala.toList.sortBy(_.name)
  }

  final case class EnclosingMethod(name: String, descriptor: String, outerClass: String)
  def enclosingMethod(className: String) = {
    val n = loadClassNode(className)
    EnclosingMethod(n.outerMethod, n.outerMethodDesc, n.outerClass)
  }

  def assertMember(node: InnerClassNode, outer: String, inner: String, name: Option[String] = None, flags: Int = Flags.ACC_PUBLIC) = {
    assertSame(node.name, name.getOrElse(s"$outer$$$inner"))
    assertSame(node.outerName, outer)
    assertSame(node.innerName, inner)
    assertSame(node.access, flags)
  }

  def assertAnonymous(node: InnerClassNode, name: String, flags: Int = Flags.ACC_PUBLIC | Flags.ACC_FINAL) = {
    assertSame(node.name, name)
    assertSame(node.outerName, null)
    assertSame(node.innerName, null)
    assertSame(node.access, flags)
  }

  def assertLocal(node: InnerClassNode, name: String, inner: String, flags: Int = Flags.ACC_PUBLIC) = {
    assertSame(node.name, name)
    assertSame(node.outerName, null)
    assertSame(node.innerName, inner)
    assertSame(node.access, flags)
  }

  def assertEnclosingMethod(className: String, outerClass: String, name: String, descriptor: String) = {
    val encl = enclosingMethod(className)
    assertSame(encl.outerClass, outerClass)
    assertSame(encl.name, name)
    assertSame(encl.descriptor, descriptor)
  }

  def assertNoEnclosingMethod(className: String) = {
    assertSame(enclosingMethod(className).outerClass, null)
  }

  def printInnerClassNodes(className: String) = {
    for (n <- innerClassNodes(className)) {
      println(s"${n.name} / ${n.outerName} / ${n.innerName} / ${n.access}")
    }
  }

  def printEnclosingMethod(className: String) = {
    val e = enclosingMethod(className)
    println(s"${e.outerClass} / ${e.name} / ${e.descriptor}")
  }

  def lambdaClass(anonfunName: String, lambdaName: String): String = {
    if (classpath.findClass(anonfunName).isDefined) anonfunName else lambdaName
  }

  def testA1() = {
    val List(b1) = innerClassNodes("A1")
    assertMember(b1, "A1", "B")
    val List(b2) = innerClassNodes("A1$B")
    assertMember(b2, "A1", "B")
  }

  def testA2() = {
    val List(b1) = innerClassNodes("A2")
    assertMember(b1, "A2", "B$")
    val List(b2) = innerClassNodes("A2$B$")
    assertMember(b2, "A2", "B$")
  }

  def testA3() = {
    def t(c: String) = {
      val List(b1, b2) = innerClassNodes(c)
      // the outer class for classes nested inside top-level modules is not the module class, but the mirror class.
      // this is a hack for java interop, handled in the backend. see BTypes.scala, comment on "Java Compatibility".
      assertMember(b1, "A3", "B1", flags = publicStatic)
      assertMember(b2, "A3", "B2$", flags = publicStatic)
    }
    t("A3$")
    // the mirror class has the same inner class attributes as the module
    // class (added when the mirror is created in the backend)
    t("A3")
  }

  def testA4() = {
    println("-- A4 --")
    printInnerClassNodes("A4")
    val fun = lambdaClass("A4$$anonfun$f$1", "A4$lambda$$f$1")
    printInnerClassNodes(fun)
    printEnclosingMethod(fun)
  }

  def testA5() = {
    val List(b1) = innerClassNodes("A5")
    assertLocal(b1, "A5$B$2$", "B$2$")
    val List(b2) = innerClassNodes("A5$B$2$")
    assertLocal(b2, "A5$B$2$", "B$2$")
    assertEnclosingMethod(
      "A5$B$2$",
      "A5", "f", "()Ljava/lang/Object;")
  }

  def testA6() = {
    val List(tt1) = innerClassNodes("A6")
    assertMember(tt1, "A6", "TT", flags = publicAbstractInterface)
    val List() = innerClassNodes("A6$class")
    val List(tt2) = innerClassNodes("A6$TT")
    assertMember(tt2, "A6", "TT", flags = publicAbstractInterface)
  }

  def testA7() = {
    val List() = innerClassNodes("A7")
  }

  def testA8() = {
    val List(tt) = innerClassNodes("A8")
    assertMember(tt, "A6", "TT", flags = publicAbstractInterface)
  }

  def testA10() = {
    val List() = innerClassNodes("A10")
  }

  def testA11() = {
    val List(ann) = innerClassNodes("A11")
    // in the java class file, the INNERCLASS attribute has more flags (public | static | abstract | interface | annotation)
    // the scala compiler has its own interpretation of java annotations ant their flags.. it only emits publicStatic.
    assertMember(ann, "JavaAnnot_1", "Ann", flags = publicStatic)
  }

  def testA13() = {
    val List(b, c) = innerClassNodes("A13")
    assertMember(b, "A12", "B$", flags = publicStatic)
    assertMember(c, "A12$B$", "C", name = Some("A12$B$C"), flags = publicStatic)
  }

  def testA14() = {
    val List(anon, k) = innerClassNodes("A14")

    assertLocal(k, "A14$K$1", "K$1")
    assertEnclosingMethod(
      "A14$K$1",
      "A14", "f", "()Ljava/lang/Object;")

    assertAnonymous(anon, "A14$$anon$1")
    assertEnclosingMethod(
      "A14$$anon$1",
      "A14", "g", "()V")
  }

  def testA15() = {
    val List(b) = innerClassNodes("A15")
    assertLocal(b, "A15$B$3", "B$3")

    val List(_, c) = innerClassNodes("A15$B$3")
    assertMember(c, "A15$B$3", "C")

    assertEnclosingMethod(
      "A15$B$3",
      "A15$", "f", "()V")
    assertNoEnclosingMethod("A15$B$3$C")
  }

  def testA16() = {
    val List(anon1, anon2, anon3, u, v) = innerClassNodes("A16")
    assertAnonymous(anon1, "A16$$anon$2")
    assertAnonymous(anon2, "A16$$anon$3")
    assertAnonymous(anon3, "A16$$anon$4")

    assertLocal(u, "A16$U$1", "U$1")
    assertLocal(v, "A16$V$1", "V$1")

    assertEnclosingMethod(
      "A16$$anon$2",
      "A16", null, null)
    assertEnclosingMethod(
      "A16$$anon$3",
      "A16", null, null)
    assertEnclosingMethod(
      "A16$$anon$4",
      "A16", null, null)

    assertEnclosingMethod(
      "A16$U$1",
      "A16", null, null)
    assertEnclosingMethod(
      "A16$V$1",
      "A16", null, null)
  }

  def testA17() = {
    val List(b, c) = innerClassNodes("A17$B$")
    assertMember(b, "A17", "B$")
    assertMember(c, "A17$B$", "C", name = Some("A17$B$C")) // not static, has an outer pointer.
  }

  def testA18() = {
    val List(anon1, anon2, a, b) = innerClassNodes("A18")
    assertAnonymous(anon1, "A18$$anon$5")
    assertAnonymous(anon2, "A18$$anon$6")

    assertLocal(a, "A18$A$1", "A$1")
    assertLocal(b, "A18$B$4", "B$4")

    assertEnclosingMethod(
      "A18$$anon$5",
      "A18", "g$1", "()V")
    assertEnclosingMethod(
      "A18$$anon$6",
      "A18", "g$1", "()V")

    assertEnclosingMethod(
      "A18$A$1",
      "A18", "g$1", "()V")
    assertEnclosingMethod(
      "A18$B$4",
      "A18", "g$1", "()V")
  }

  def testA19() = {
    println("-- A19 --")

    printInnerClassNodes("A19")

    val fun1 = lambdaClass("A19$$anonfun$1", "A19$lambda$1")
    val fun2 = lambdaClass("A19$$anonfun$2", "A19$lambda$2")
    val fun3 = lambdaClass("A19$$anonfun$3", "A19$lambda$3")

    printInnerClassNodes(fun1)
    printInnerClassNodes(fun2)
    printInnerClassNodes(fun3)

    printEnclosingMethod(fun1)
    printEnclosingMethod(fun2)
    printEnclosingMethod(fun3)
  }

  def testA20() = {
    println("-- A20 --")

    printInnerClassNodes("A20")

    val fun1 = lambdaClass("A20$$anonfun$4", "A20$lambda$1")
    val fun2 = lambdaClass("A20$$anonfun$4$$anonfun$apply$1", "A20$lambda$$$anonfun$5$1")
    val fun3 = lambdaClass("A20$$anonfun$4$$anonfun$apply$3", "A20$lambda$$$anonfun$5$2")
    val fun4 = lambdaClass("A20$$anonfun$4$$anonfun$apply$3$$anonfun$apply$2", "A20$lambda$$$anonfun$7$1")

    println("fun1: attribute for itself and the two child closures `() => ()` and `() => () => 1`")
    printInnerClassNodes(fun1)
    println("fun2 () => (): itself and the outer closure")
    printInnerClassNodes(fun2)
    println("fun3 () => () => (): itself, the outer closure and its child closure")
    printInnerClassNodes(fun3)
    println("fun4: () => 1: itself and the two outer closures")
    printInnerClassNodes(fun4)

    println("enclosing: nested closures have the apply method of the outer closure")
    printEnclosingMethod(fun1)
    printEnclosingMethod(fun2)
    printEnclosingMethod(fun3)
    printEnclosingMethod(fun4)
  }

  def testA21() = {
    val List(i1c, i2c, i3c, j1) = innerClassNodes("A21")
    assertMember(i1c, "A21", "I1")
    assertMember(i2c, "A21", "I2", flags = publicStatic)
    assertMember(i3c, "A21", "I3$", flags = publicStatic)
    assertLocal(j1, "A21$J1$1", "J1$1")

    val List(i2m, i3m, j3, j4, j5) = innerClassNodes("A21$")
    assertMember(i2m, "A21", "I2", flags = publicStatic)
    assertMember(i3m, "A21", "I3$", flags = publicStatic)
    assertLocal(j3, "A21$J3$1", "J3$1")
    assertLocal(j4, "A21$J4$1", "J4$1")
    assertLocal(j5, "A21$J5$1", "J5$1") // non-static!

    val List(i3x, j2x) = innerClassNodes("A21$I3$J2")
    assertMember(j2x, "A21$I3$", "J2", name = Some("A21$I3$J2"), flags = publicStatic)

    assertNoEnclosingMethod("A21$I3$J2")
    assertEnclosingMethod("A21$J3$1", "A21$", "g", "()V")
    assertEnclosingMethod("A21$J4$1", "A21$", null, null)
    assertEnclosingMethod("A21$J5$1", "A21$", null, null)
  }

  def testA22() = {
    val List(cc) = innerClassNodes("A22$C")
    assertMember(cc, "A22", "C")
    val List(cm, d) = innerClassNodes("A22$C$")
    assertMember(cm, "A22", "C$")
    assertMember(d, "A22$C$", "D", name = Some("A22$C$D"))
  }

  def testA23() {
    val List(c, d, e, f, g) = innerClassNodes("A23")
    assertMember(c, "Java_A_1", "C", flags = publicStatic)
    assertMember(d, "Java_A_1$C", "D", flags = publicStatic)
    assertMember(e, "Java_A_1$C", "E")
    assertMember(f, "Java_A_1", "F")
    assertMember(g, "Java_A_1$F", "G")
  }

  def testA24() {
    val List(defsCls, abs, conc, defsApi, defsApiImpl) = innerClassNodes("A24$DefinitionsClass")
    assertMember(defsCls, "A24", "DefinitionsClass")
    assertMember(abs, "A24$DefinitionsClass", "Abs$")
    assertMember(conc, "A24$DefinitionsClass", "Conc$")
    assertMember(defsApi, "A24Base", "DefinitionsApi", flags = publicAbstractInterface)
    assertMember(defsApiImpl, "A24Base", "DefinitionsApi$class", flags = Flags.ACC_PUBLIC | Flags.ACC_ABSTRACT)
  }

  def show(): Unit = {
    testA1()
    testA2()
    testA3()
    testA4()
    testA5()
    testA6()
    testA7()
    testA8()
    testA10()
    testA11()
    testA13()
    testA14()
    testA15()
    testA16()
    testA17()
    testA18()
    testA19()
    testA20()
    testA21()
    testA22()
    testA23()
    testA24()
  }
}
