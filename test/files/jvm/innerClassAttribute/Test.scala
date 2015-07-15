import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.{ClassNode, InnerClassNode}
import asm.{Opcodes => Flags}
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  // Helpful for debugging the test:
  // println(new java.io.File(classpath.asURLs.head.toURI).list().sorted.mkString("\n"))

  def assertSame(a: Any, b: Any) = {
    assert(a == b, s"\na: $a\nb: $b")
  }

  val publicStatic = Flags.ACC_PUBLIC | Flags.ACC_STATIC
  val publicAbstractInterface = Flags.ACC_PUBLIC | Flags.ACC_ABSTRACT | Flags.ACC_INTERFACE

  def innerClassNodes(className: String): List[InnerClassNode] = {
    loadClassNode(className).innerClasses.asScala.toList.sortBy(_.name)
  }

  def ownInnerClassNode(n: String) = innerClassNodes(n).filter(_.name == n).head

  def testInner(cls: String, fs: (InnerClassNode => Unit)*) = {
    val ns = innerClassNodes(cls)
    assert(ns.length == fs.length, ns)
    (ns zip fs.toList) foreach { case (n, f) => f(n) }
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
    // the inner class entries for top-level object members are in the companion class, so nothing in the module class
    val List() = innerClassNodes("A3$")

    // inner class entries in the companion class (a backend-generated mirror class in this case)
    val List(b1, b2) = innerClassNodes("A3")
    // the outer class for classes nested inside top-level modules is not the module class, but the mirror class.
    // this is a hack for java interop, handled in the backend. see BTypes.scala, comment on "Java Compatibility".
    assertMember(b1, "A3", "B1", flags = publicStatic)
    assertMember(b2, "A3", "B2$", flags = publicStatic)
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
    assertMember(ann, "JavaAnnot_1", "Ann", flags = publicAbstractInterface | Flags.ACC_STATIC | Flags.ACC_ANNOTATION)
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
    // no member classes, only anonymous / local. these are nested in the module class, not the companion.
    val List() = innerClassNodes("A15")

    val List(b) = innerClassNodes("A15$")
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

    assertLocal(a, "A18$A$2", "A$2")
    assertLocal(b, "A18$B$4", "B$4")

    assertEnclosingMethod(
      "A18$$anon$5",
      "A18", "g$1", "()V")
    assertEnclosingMethod(
      "A18$$anon$6",
      "A18", "g$1", "()V")

    assertEnclosingMethod(
      "A18$A$2",
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
    val fun2 = lambdaClass("A20$$anonfun$4$$anonfun$apply$1", "A20$lambda$$$nestedInAnonfun$5$1")
    val fun3 = lambdaClass("A20$$anonfun$4$$anonfun$apply$2", "A20$lambda$$$nestedInAnonfun$5$2")
    val fun4 = lambdaClass("A20$$anonfun$4$$anonfun$apply$2$$anonfun$apply$3", "A20$lambda$$$nestedInAnonfun$7$1")

    println("fun1: attribute for itself and the two child closures `() => ()` and `() => () => 1`")
    printInnerClassNodes(fun1)
    println("fun2 () => (): itself and the outer closure")
    printInnerClassNodes(fun2)
    println("fun3 () => () => (): itself, the outer closure and its child closure")
    printInnerClassNodes(fun3)
    println("fun4: () => 1: itself and the two outer closures")
    printInnerClassNodes(fun4)

    println("enclosing: nested closures have outer class defined, but no outer method")
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

    val List(j3, j4, j5) = innerClassNodes("A21$")
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
    val List(defsCls, abs, conc, defsApi) = innerClassNodes("A24$DefinitionsClass")
    assertMember(defsCls, "A24", "DefinitionsClass")
    assertMember(abs, "A24$DefinitionsClass", "Abs$")
    assertMember(conc, "A24$DefinitionsClass", "Conc$")
    assertMember(defsApi, "A24Base", "DefinitionsApi", flags = publicAbstractInterface)
  }

  def testSI_9105() {
    val isDelambdafyMethod = classpath.findClass("SI_9105$lambda$1").isDefined
    if (isDelambdafyMethod) {
      assertEnclosingMethod  ("SI_9105$A$3"          , "SI_9105", null , null)
      assertEnclosingMethod  ("SI_9105$B$5"          , "SI_9105", "m$1", "()Ljava/lang/Object;")
      assertEnclosingMethod  ("SI_9105$C$1"          , "SI_9105", null , null)
      assertEnclosingMethod  ("SI_9105$D$1"          , "SI_9105", "met", "()Lscala/Function1;")
      assertEnclosingMethod  ("SI_9105$E$1"          , "SI_9105", "m$3", "()Ljava/lang/Object;")
      assertEnclosingMethod  ("SI_9105$F$1"          , "SI_9105", "met", "()Lscala/Function1;")
      assertNoEnclosingMethod("SI_9105$lambda$$met$1")
      assertNoEnclosingMethod("SI_9105$lambda$1")
      assertNoEnclosingMethod("SI_9105")

      assertLocal(innerClassNodes("SI_9105$A$3").head, "SI_9105$A$3", "A$3")
      assertLocal(innerClassNodes("SI_9105$B$5").head, "SI_9105$B$5", "B$5")
      assertLocal(innerClassNodes("SI_9105$C$1").head, "SI_9105$C$1", "C$1")
      assertLocal(innerClassNodes("SI_9105$D$1").head, "SI_9105$D$1", "D$1")
      assertLocal(innerClassNodes("SI_9105$E$1").head, "SI_9105$E$1", "E$1")
      assertLocal(innerClassNodes("SI_9105$F$1").head, "SI_9105$F$1", "F$1")

      // by-name
      assertEnclosingMethod("SI_9105$G$1", "SI_9105", null , null)
      assertEnclosingMethod("SI_9105$H$1", "SI_9105", "m$2", "()Ljava/lang/Object;")
      assertEnclosingMethod("SI_9105$I$1", "SI_9105", null , null)
      assertEnclosingMethod("SI_9105$J$1", "SI_9105", "bnM", "()I")
      assertEnclosingMethod("SI_9105$K$2", "SI_9105", "m$4", "()Ljava/lang/Object;")
      assertEnclosingMethod("SI_9105$L$1", "SI_9105", "bnM", "()I")

      assert(innerClassNodes("SI_9105$lambda$$met$1").isEmpty)
      assert(innerClassNodes("SI_9105$lambda$1").isEmpty)
      assert(innerClassNodes("SI_9105").length == 12) // the 12 local classes
    } else {
      // comment in innerClassAttribute/Classes_1.scala explains the difference between A / C and D / F.
      assertEnclosingMethod  ("SI_9105$$anonfun$5$A$3"    , "SI_9105$$anonfun$5"    , null          , null)
      assertEnclosingMethod  ("SI_9105$$anonfun$5$B$5"    , "SI_9105$$anonfun$5"    , "m$1"         , "()Ljava/lang/Object;")
      assertEnclosingMethod  ("SI_9105$$anonfun$5$C$1"    , "SI_9105$$anonfun$5"    , null          , null)
      assertEnclosingMethod  ("SI_9105$$anonfun$met$1$D$1", "SI_9105$$anonfun$met$1", null          , null)
      assertEnclosingMethod  ("SI_9105$$anonfun$met$1$E$1", "SI_9105$$anonfun$met$1", "m$3"         , "()Ljava/lang/Object;")
      assertEnclosingMethod  ("SI_9105$$anonfun$met$1$F$1", "SI_9105$$anonfun$met$1", null          , null)
      assertEnclosingMethod  ("SI_9105$$anonfun$5"        , "SI_9105"               , null          , null)
      assertEnclosingMethod  ("SI_9105$$anonfun$met$1"    , "SI_9105"               , "met"         , "()Lscala/Function1;")
      assertNoEnclosingMethod("SI_9105")

      assertLocal(ownInnerClassNode("SI_9105$$anonfun$5$A$3"),     "SI_9105$$anonfun$5$A$3"    , "A$3")
      assertLocal(ownInnerClassNode("SI_9105$$anonfun$5$B$5"),     "SI_9105$$anonfun$5$B$5"    , "B$5")
      assertLocal(ownInnerClassNode("SI_9105$$anonfun$5$C$1"),     "SI_9105$$anonfun$5$C$1"    , "C$1")
      assertLocal(ownInnerClassNode("SI_9105$$anonfun$met$1$D$1"), "SI_9105$$anonfun$met$1$D$1", "D$1")
      assertLocal(ownInnerClassNode("SI_9105$$anonfun$met$1$E$1"), "SI_9105$$anonfun$met$1$E$1", "E$1")
      assertLocal(ownInnerClassNode("SI_9105$$anonfun$met$1$F$1"), "SI_9105$$anonfun$met$1$F$1", "F$1")

      // by-name
      assertEnclosingMethod("SI_9105$$anonfun$6$G$1", "SI_9105$$anonfun$6", null, null)
      assertEnclosingMethod("SI_9105$$anonfun$6$H$1", "SI_9105$$anonfun$6", "m$2", "()Ljava/lang/Object;")
      assertEnclosingMethod("SI_9105$$anonfun$6$I$1", "SI_9105$$anonfun$6", null, null)
      assertEnclosingMethod("SI_9105$$anonfun$bnM$1$J$1", "SI_9105$$anonfun$bnM$1", null, null)
      assertEnclosingMethod("SI_9105$$anonfun$bnM$1$K$2", "SI_9105$$anonfun$bnM$1", "m$4", "()Ljava/lang/Object;")
      assertEnclosingMethod("SI_9105$$anonfun$bnM$1$L$1", "SI_9105$$anonfun$bnM$1", null, null)

      assertAnonymous(ownInnerClassNode("SI_9105$$anonfun$5"), "SI_9105$$anonfun$5")
      assertAnonymous(ownInnerClassNode("SI_9105$$anonfun$met$1"), "SI_9105$$anonfun$met$1")

      assert(innerClassNodes("SI_9105$$anonfun$5").length == 4)     // itself and three of the local classes
      assert(innerClassNodes("SI_9105$$anonfun$met$1").length == 4) // itself and three of the local classes
      assert(innerClassNodes("SI_9105").length == 4)                // the four anon funs
    }
  }

  def testSI_9124() {
    val classes: Map[String, String] = {
      List("SI_9124$$anon$10",
           "SI_9124$$anon$11",
           "SI_9124$$anon$12",
           "SI_9124$$anon$8",
           "SI_9124$$anon$9",
           "SI_9124$O$$anon$13").map({ name =>
        val node = loadClassNode(name)
        val fMethod = node.methods.asScala.find(_.name.startsWith("f")).get.name
        (fMethod, node.name)
      }).toMap
    }

    // println(classes)

    assertNoEnclosingMethod("SI_9124$A")
    assertEnclosingMethod(classes("f1"), "SI_9124", null, null)
    assertEnclosingMethod(classes("f2"), "SI_9124", "f", "()LSI_9124$A;")
    assertEnclosingMethod(classes("f3"), "SI_9124", null, null)
    assertEnclosingMethod(classes("f4"), "SI_9124$O$", null, null)
    assertEnclosingMethod(classes("f5"), "SI_9124", null, null)
    assertEnclosingMethod(classes("f6"), "SI_9124", null, null)
    assertNoEnclosingMethod("SI_9124$O$")

    assertMember(ownInnerClassNode("SI_9124$A"), "SI_9124", "A", flags = publicAbstractInterface)
    classes.values.foreach(n => assertAnonymous(ownInnerClassNode(n), n))
    assertMember(ownInnerClassNode("SI_9124$O$"), "SI_9124", "O$")
  }

  def testImplClassesTopLevel() {
    val classes = List(
      "ImplClassesAreTopLevel$$anon$14",
      "ImplClassesAreTopLevel$$anon$15",
      "ImplClassesAreTopLevel$$anon$16",
      "ImplClassesAreTopLevel$B1$class",
      "ImplClassesAreTopLevel$B1",
      "ImplClassesAreTopLevel$B2$1$class",
      "ImplClassesAreTopLevel$B2$1",
      "ImplClassesAreTopLevel$B3$1$class",
      "ImplClassesAreTopLevel$B3$1",
      "ImplClassesAreTopLevel$B4$class",
      "ImplClassesAreTopLevel$B4$1",
      "ImplClassesAreTopLevel$class",
      "ImplClassesAreTopLevel")

    classes.filter(_.endsWith("$class")).foreach(assertNoEnclosingMethod)
    classes.flatMap(innerClassNodes).foreach(icn => assert(!icn.name.endsWith("$class"), icn))

    assertNoEnclosingMethod("ImplClassesAreTopLevel$B1") // member, no encl meth attr

    // no encl meth, but encl class
    List("ImplClassesAreTopLevel$B2$1", "ImplClassesAreTopLevel$B3$1",
         "ImplClassesAreTopLevel$$anon$14", "ImplClassesAreTopLevel$$anon$15").foreach(assertEnclosingMethod(_, "ImplClassesAreTopLevel", null, null))

    // encl meth n
    List("ImplClassesAreTopLevel$B4$1", "ImplClassesAreTopLevel$$anon$16").foreach(assertEnclosingMethod(_, "ImplClassesAreTopLevel", "n", "()Ljava/lang/Object;"))

    val an14 = assertAnonymous(_: InnerClassNode, "ImplClassesAreTopLevel$$anon$14")
    val an15 = assertAnonymous(_: InnerClassNode, "ImplClassesAreTopLevel$$anon$15")
    val an16 = assertAnonymous(_: InnerClassNode, "ImplClassesAreTopLevel$$anon$16")
    val b1 = assertMember(_: InnerClassNode, "ImplClassesAreTopLevel", "B1", flags = publicAbstractInterface)
    val b2 = assertLocal(_ : InnerClassNode, "ImplClassesAreTopLevel$B2$1", "B2$1", flags = publicAbstractInterface)
    val b3 = assertLocal(_ : InnerClassNode, "ImplClassesAreTopLevel$B3$1", "B3$1", flags = publicAbstractInterface)
    val b4 = assertLocal(_ : InnerClassNode, "ImplClassesAreTopLevel$B4$1", "B4$1", flags = publicAbstractInterface)

    testInner("ImplClassesAreTopLevel$$anon$14", an14, b3)
    testInner("ImplClassesAreTopLevel$$anon$15", an15, b2)
    testInner("ImplClassesAreTopLevel$$anon$16", an16, b4)

    testInner("ImplClassesAreTopLevel$B1$class", b1)
    testInner("ImplClassesAreTopLevel$B2$1$class", b2)
    testInner("ImplClassesAreTopLevel$B3$1$class", b3)
    testInner("ImplClassesAreTopLevel$B4$class", b4)

    testInner("ImplClassesAreTopLevel$B1", b1)
    testInner("ImplClassesAreTopLevel$B2$1", b2)
    testInner("ImplClassesAreTopLevel$B3$1", b3)
    testInner("ImplClassesAreTopLevel$B4$1", b4)

    testInner("ImplClassesAreTopLevel$class", an14, an15, an16)
    testInner("ImplClassesAreTopLevel", an14, an15, an16, b1, b2, b3, b4)
  }

  def testSpecializedClassesTopLevel() {
    val cls = List(
      "SpecializedClassesAreTopLevel$A$mcI$sp",
      "SpecializedClassesAreTopLevel$A",
      "SpecializedClassesAreTopLevel$T$",
      "SpecializedClassesAreTopLevel$T$B$mcI$sp",
      "SpecializedClassesAreTopLevel$T$B",
      "SpecializedClassesAreTopLevel")

    // all classes are members, no local (can't test local, they crash in specialize)
    cls.foreach(assertNoEnclosingMethod)
    cls.flatMap(innerClassNodes).foreach(icn => assert(!icn.name.endsWith("$sp"), icn))

    val a = assertMember(_: InnerClassNode, "SpecializedClassesAreTopLevel", "A")
    val t = assertMember(_: InnerClassNode, "SpecializedClassesAreTopLevel", "T$")
    val b = assertMember(_: InnerClassNode, "SpecializedClassesAreTopLevel$T$", "B", Some("SpecializedClassesAreTopLevel$T$B"))

    List("SpecializedClassesAreTopLevel$A$mcI$sp", "SpecializedClassesAreTopLevel$A").foreach(testInner(_, a))
    testInner("SpecializedClassesAreTopLevel", a, t)
    List("SpecializedClassesAreTopLevel$T$", "SpecializedClassesAreTopLevel$T$B$mcI$sp", "SpecializedClassesAreTopLevel$T$B").foreach(testInner(_, t, b))
  }

  def testNestedInValueClass() {
    List(
      "NestedInValueClass",
      "NestedInValueClass$",
      "NestedInValueClass$A",
      "NestedInValueClass$A$",
      "NestedInValueClass$A$B").foreach(assertNoEnclosingMethod)

    assertEnclosingMethod("NestedInValueClass$A$C$2", "NestedInValueClass$A$", "f", "()Ljava/lang/Object;")

    type I = InnerClassNode
    val a  = assertMember(_: I, "NestedInValueClass", "A", flags = publicStatic | Flags.ACC_FINAL)
    val am = assertMember(_: I, "NestedInValueClass", "A$", flags = publicStatic)
    val b = assertMember(_: I, "NestedInValueClass$A$", "B", Some("NestedInValueClass$A$B"), flags = publicStatic)
    val c = assertLocal(_: I, "NestedInValueClass$A$C$2", "C$2")

    testInner("NestedInValueClass$")
    testInner("NestedInValueClass", a, am)
    testInner("NestedInValueClass$A$B", am, b)
    testInner("NestedInValueClass$A$C$2", am, c)

    val isDelambdafyMethod = classpath.findClass("NestedInValueClass$A$lambda$$f$extension$1").isDefined
    if (isDelambdafyMethod) {
      List(
        "NestedInValueClass$A$lambda$$g$2$1",
        "NestedInValueClass$A$lambda$$f$extension$1",
        "NestedInValueClass$A$lambda$$$nestedInAnonfun$13$1",
        "NestedInValueClass$A$lambda$$NestedInValueClass$A$$$nestedInAnonfun$15$1").foreach(assertNoEnclosingMethod)
      testInner("NestedInValueClass$A", a, am)
      testInner("NestedInValueClass$A$", a, am, b, c)
      testInner("NestedInValueClass$A$lambda$$g$2$1", am)
      testInner("NestedInValueClass$A$lambda$$f$extension$1", am)
      testInner("NestedInValueClass$A$lambda$$$nestedInAnonfun$13$1", am)
      testInner("NestedInValueClass$A$lambda$$NestedInValueClass$A$$$nestedInAnonfun$15$1", am)
    } else {
      assertEnclosingMethod("NestedInValueClass$A$$anonfun$g$2$1"                         , "NestedInValueClass$A"                       , null, null)
      assertEnclosingMethod("NestedInValueClass$A$$anonfun$g$2$1$$anonfun$apply$4"        , "NestedInValueClass$A$$anonfun$g$2$1"        , null, null)
      assertEnclosingMethod("NestedInValueClass$A$$anonfun$f$extension$1"                 , "NestedInValueClass$A"                       , "f", "()Lscala/collection/immutable/List;")
      assertEnclosingMethod("NestedInValueClass$A$$anonfun$f$extension$1$$anonfun$apply$5", "NestedInValueClass$A$$anonfun$f$extension$1", null, null)

      val gfun    = assertAnonymous(_: I, "NestedInValueClass$A$$anonfun$g$2$1")
      val ffun    = assertAnonymous(_: I, "NestedInValueClass$A$$anonfun$f$extension$1")
      val gfunfun = assertAnonymous(_: I, "NestedInValueClass$A$$anonfun$g$2$1$$anonfun$apply$4")
      val ffunfun = assertAnonymous(_: I, "NestedInValueClass$A$$anonfun$f$extension$1$$anonfun$apply$5")

      testInner("NestedInValueClass$A", a, am, ffun, gfun)
      testInner("NestedInValueClass$A$", a, am, ffun, gfun, b, c)
      testInner("NestedInValueClass$A$$anonfun$g$2$1", a, am, gfun, gfunfun)
      testInner("NestedInValueClass$A$$anonfun$g$2$1$$anonfun$apply$4", am, gfun, gfunfun)
      testInner("NestedInValueClass$A$$anonfun$f$extension$1", a, am, ffun, ffunfun)
      testInner("NestedInValueClass$A$$anonfun$f$extension$1$$anonfun$apply$5", am, ffun, ffunfun)
    }
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
    testSI_9105()
    testSI_9124()
    testImplClassesTopLevel()
    testSpecializedClassesTopLevel()
    testNestedInValueClass()
  }
}
