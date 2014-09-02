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

  def assertEnclosingMethod(enclosingMethod: EnclosingMethod, outerClass: String, name: String, descriptor: String) = {
    assertSame(enclosingMethod.outerClass, outerClass)
    assertSame(enclosingMethod.name, name)
    assertSame(enclosingMethod.descriptor, descriptor)
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
    val List(an1) = innerClassNodes("A4")
    assertAnonymous(an1, "A4$$anonfun$f$1")
    val List(an2) = innerClassNodes("A4$$anonfun$f$1")
    assertAnonymous(an2, "A4$$anonfun$f$1")
    assertEnclosingMethod(
      enclosingMethod("A4$$anonfun$f$1"),
      "A4", "f", "(Lscala/collection/immutable/List;)Lscala/collection/immutable/List;")
  }

  def testA5() = {
    val List(b1) = innerClassNodes("A5")
    assertLocal(b1, "A5$B$2$", "B$2$")
    val List(b2) = innerClassNodes("A5$B$2$")
    assertLocal(b2, "A5$B$2$", "B$2$")
    assertEnclosingMethod(
      enclosingMethod("A5$B$2$"),
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
      enclosingMethod("A14$K$1"),
      "A14", "f", "()Ljava/lang/Object;")

    assertAnonymous(anon, "A14$$anon$1")
    assertEnclosingMethod(
      enclosingMethod("A14$$anon$1"),
      "A14", "g", "()V")
  }

  def testA15() = {
    val List(b) = innerClassNodes("A15")
    assertLocal(b, "A15$B$3", "B$3", flags = publicStatic)

    val List(_, c) = innerClassNodes("A15$B$3")
    // TODO this is a bug in the backend, C should be a member. Instead, its outerClass is null
    // assertMember(c, "A15$B$3", "C")
    assertLocal(c, "A15$B$3$C", "C")
  }

  def testA16() = {
    val List(anon1, anon2, u, v) = innerClassNodes("A16")
    // TODO there's a bug in the backend: anon$2 has outerClass A16, but anonymous classes should have outerClass null
    // assertAnonymous(anon1, "A16$$anon$2")
    assertMember(anon1, "A16", null, name = Some("A16$$anon$2"), flags = Flags.ACC_PUBLIC | Flags.ACC_FINAL)
    assertAnonymous(anon2, "A16$$anon$3")
    // TODO this is a bug in the backend, U should not be a member, its outerClass should be null
    // assertLocal(u, "A16$U$1", "U$1")
    assertMember(u, "A16", "U$1")
    assertLocal(v, "A16$V$1", "V$1")

    assertEnclosingMethod(
      enclosingMethod("A16$$anon$2"),
      "A16", "<init>", "()V")
    assertEnclosingMethod(
      enclosingMethod("A16$$anon$3"),
      "A16", "<init>", "()V")
    // TODO this is a bug, there should be an enclosingMethod attribute in U
    // assertEnclosingMethod(
    //   enclosingMethod("A16$U$1"),
    //   "A16", "<init>", "()V")
    assertEnclosingMethod(
      enclosingMethod("A16$V$1"),
      "A16", "<init>", "()V")
  }

  def testA17() = {
    val List(b, c) = innerClassNodes("A17$B$")
    assertMember(b, "A17", "B$")
    // TODO this is a bug, should not be static.
    assertMember(c, "A17$B$", "C", name = Some("A17$B$C"), flags = publicStatic) // (should be) not static, has an outer pointer.
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
  }
}
