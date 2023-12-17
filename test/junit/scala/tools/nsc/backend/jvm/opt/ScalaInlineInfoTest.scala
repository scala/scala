package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.CollectionConverters._
import scala.collection.immutable.TreeMap
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.BTypes.{EmptyInlineInfo, InlineInfo, MethodInlineInfo}
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

@RunWith(classOf[JUnit4])
class ScalaInlineInfoTest extends BytecodeTesting {
  override def compilerArgs = "-opt:none"
  import compiler._

  def inlineInfo(c: ClassNode): InlineInfo = c.attrs.asScala.collect({ case a: InlineInfoAttribute => a.inlineInfo }).head

  def mapDiff[A, B](a: collection.Map[A, B], b: collection.Map[A, B]) = {
    val r = new StringBuilder
    for ((a, av) <- a) {
      if (!b.contains(a)) r.append(s"missing in b: $a\n")
      else if (av != b(a)) r.append(s"different for $a: $av != ${b(a)}\n")
    }
    for (b <- b.keys.toList diff a.keys.toList) {
      r.append(s"missing in a: $b\n")
    }
    r.toString
  }

  def assertSameMethods(c: ClassNode, nameAndSigs: collection.Set[String]): Unit = {
    val r = new StringBuilder
    val inClass = c.methods.iterator.asScala.map(m => m.name + m.desc).toSet
    for (m <- inClass.diff(nameAndSigs)) r.append(s"method in classfile found, but no inline info: $m")
    for (m <- nameAndSigs.diff(inClass)) r.append(s"inline info found, but no method in classfile: $m")
    assert(r.isEmpty, r.toString)
  }

  @Test
  def traitMembersInlineInfo(): Unit = {
    val code =
      """trait T {
        |  def f1 = 1                   // concrete method
        |  private def f2 = 1           // default method only (not in subclass)
        |  def f3 = {
        |    def nest = 0               // nested method (does not end up in the interface)
        |    nest
        |  }
        |
        |  @inline
        |  def f4 = super.toString      // super accessor
        |
        |  object O                     // module accessor (method is generated)
        |  final def f5 = {
        |    object L { val x = 0 }     // nested module (just flattened out)
        |    L.x
        |  }
        |
        |  @noinline
        |  def f6: Int                  // abstract method
        |
        |  // fields
        |
        |  val x1 = 0
        |  var y2 = 0
        |  var x3: Int
        |  lazy val x4 = 0
        |
        |  final val x5 = 0
        |}
        |class C extends T {
        |  def f6 = 0
        |  var x3 = 0
        |}
      """.stripMargin

    val cs @ List(c, t, tl, to) = compileClasses(code)
    val infoT = inlineInfo(t)
    val expectT = EmptyInlineInfo.copy(methodInfos = TreeMap(
        (("O", "()LT$O$;"),                                                 MethodInlineInfo()),
        (("T$$super$toString", "()Ljava/lang/String;"),                     MethodInlineInfo(effectivelyFinal = true)),
        (("T$_setter_$x1_$eq", "(I)V"),                                     MethodInlineInfo()),
        (("f1", "()I"),                                                     MethodInlineInfo()),
        (("f1$", "(LT;)I"),                                                 MethodInlineInfo(effectivelyFinal = true)),
        (("f2", "()I"),                                                     MethodInlineInfo(effectivelyFinal = true)), // no static impl method for private method f2
        (("f3", "()I"),                                                     MethodInlineInfo()),
        (("f3$", "(LT;)I"),                                                 MethodInlineInfo(effectivelyFinal = true)),
        (("f4", "()Ljava/lang/String;"),                                    MethodInlineInfo(annotatedInline = true)),
        (("f4$", "(LT;)Ljava/lang/String;"),                                MethodInlineInfo(effectivelyFinal = true, annotatedInline = true)),
        (("f5", "()I"),                                                     MethodInlineInfo(effectivelyFinal = true)),
        (("f5$", "(LT;)I"),                                                 MethodInlineInfo(effectivelyFinal = true)),
        (("f6", "()I"),                                                     MethodInlineInfo(annotatedNoInline = true)), // no static impl method for abstract method f6
        (("x1", "()I"),                                                     MethodInlineInfo()),
        (("y2", "()I"),                                                     MethodInlineInfo()),
        (("y2_$eq", "(I)V"),                                                MethodInlineInfo()),
        (("x3", "()I"),                                                     MethodInlineInfo()),
        (("x3_$eq", "(I)V"),                                                MethodInlineInfo()),
        (("x4", "()I"),                                                     MethodInlineInfo()),
        (("x4$", "(LT;)I"),                                                 MethodInlineInfo(effectivelyFinal = true)),
        (("x5", "()I"),                                                     MethodInlineInfo(effectivelyFinal = true)),
        (("x5$", "(LT;)I"),                                                 MethodInlineInfo(effectivelyFinal = true)),
        (("L$2", "(Lscala/runtime/LazyRef;)LT$L$1$;"),                      MethodInlineInfo(effectivelyFinal = true)),
        (("nest$1", "()I"),                                                 MethodInlineInfo(effectivelyFinal = true)),
        (("$init$", "(LT;)V"),                                              MethodInlineInfo(effectivelyFinal = true)),
        (("L$lzycompute$1", "(Lscala/runtime/LazyRef;)LT$L$1$;"),           MethodInlineInfo(effectivelyFinal = true))
      ))

    assert(infoT == expectT, mapDiff(expectT.methodInfos, infoT.methodInfos) + infoT)
    assertSameMethods(t, expectT.methodInfos.keySet.map(x => x._1 + x._2))

    val infoC = inlineInfo(c)
    val expectC = EmptyInlineInfo.copy(methodInfos = TreeMap(
      ("O", "()LT$O$;")                             -> MethodInlineInfo(effectivelyFinal = true),
      ("f1", "()I")                                 -> MethodInlineInfo(),
      ("f3", "()I")                                 -> MethodInlineInfo(),
      ("f4", "()Ljava/lang/String;")                -> MethodInlineInfo(annotatedInline = true),
      ("f5", "()I")                                 -> MethodInlineInfo(effectivelyFinal = true),
      ("f6", "()I")                                 -> MethodInlineInfo(),
      ("x1", "()I")                                 -> MethodInlineInfo(),
      ("T$_setter_$x1_$eq", "(I)V")                 -> MethodInlineInfo(),
      ("y2", "()I")                                 -> MethodInlineInfo(),
      ("y2_$eq", "(I)V")                            -> MethodInlineInfo(),
      ("x3", "()I")                                 -> MethodInlineInfo(),
      ("x3_$eq", "(I)V")                            -> MethodInlineInfo(),
      ("x4$lzycompute", "()I")                      -> MethodInlineInfo(effectivelyFinal = true),
      ("x4", "()I")                                 -> MethodInlineInfo(),
      ("T$$super$toString", "()Ljava/lang/String;") -> MethodInlineInfo(effectivelyFinal = true),
      ("<init>", "()V")                             -> MethodInlineInfo(),
      ("O$lzycompute$1", "()V")                     -> MethodInlineInfo(effectivelyFinal = true)
    ))

    assert(infoC == expectC, mapDiff(expectC.methodInfos, infoC.methodInfos) + infoC)
    assertSameMethods(c, expectC.methodInfos.keySet.map(x => x._1 + x._2))
  }

  @Test
  def inlineInfoSam(): Unit = {
    val code =
      """@FunctionalInterface trait C { // expected to be seen as sam: g(I)I
        |  def f = 0
        |  def g(x: Int): Int
        |  val foo = "hi"
        |}
        |@FunctionalInterface abstract class D { // not actually a functional interface, but scalac doesn't error
        |  val biz: Int
        |}
        |@FunctionalInterface trait T { // expected to be seen as sam: h(Ljava/lang/String;)I
        |  def h(a: String): Int
        |}
        |@FunctionalInterface trait E extends T { // expected to be seen as sam: h(Ljava/lang/String;)I
        |  def hihi(x: Int) = x
        |}
        |@FunctionalInterface class F extends T { // not actually a functional interface, but scalac doesn't error
        |  def h(a: String) = 0
        |}
        |@FunctionalInterface trait U {
        |  def conc() = 10
        |  def nullary: Int
        |}
        |trait V { // not annotated @FunctionalInterface, therefore not treated as SAM by the optimizer
        |  def h(a: String): Int
        |}
      """.stripMargin
    val cs = compileClasses(code)
    val sams = cs.map(c => (c.name, inlineInfo(c).sam))
    assertEquals(sams,
      List(
        ("C",Some("g(I)I")),
        ("D",None),
        ("E",Some("h(Ljava/lang/String;)I")),
        ("F",None),
        ("T",Some("h(Ljava/lang/String;)I")),
        ("U",None),
        ("V", None)))
  }

  @Test
  def lzyComputeInlineInfo(): Unit = {
    val code = "class C { object O }"
    val List(c, om) = compileClasses(code)
    val infoC = inlineInfo(c)
    val expected = Map(
      ("<init>", "()V")         -> MethodInlineInfo(),
      ("O$lzycompute$1", "()V") -> MethodInlineInfo(effectivelyFinal = true),
      ("O", "()LC$O$;")         -> MethodInlineInfo(effectivelyFinal = true))
    assert(infoC.methodInfos == expected, mapDiff(infoC.methodInfos, expected))
    assertSameMethods(c, expected.keySet.map(x => x._1 + x._2))
  }

  @Test
  def looksLikeForwarderTest(): Unit = {
    import global.genBCode.postProcessor.backendUtils._

    val code =
      """trait T { def a = 0 }
        |class C(x: Int, y: Long) extends T {
        |  def t1 = {
        |    val f = (b: Byte, i: Int) => i + b
        |    f(1, 2)
        |  }
        |}
        |object C {
        |  def make(x: Int, y: java.lang.Long) = new C(x, y)
        |  def foo(s: String) = {
        |    val k = s"$s-$s"
        |    println(k)
        |  }
        |}
      """.stripMargin

    val List(c, cm, t) =  compileClasses(code)

    def tst(c: ClassNode, m: String, r: Int): Unit = assertEquals(looksLikeForwarderOrFactoryOrTrivial(getAsmMethod(c, m), c.name, allowPrivateCalls = true), r)
    tst(c, "a", 4)
    tst(c, "$anonfun$t1$1$adapted", 3)
    tst(c, "$anonfun$t1$1", 1)
    tst(c, "t1", -1)

    tst(t, "a$", 4)
    tst(t, "a", 1)

    tst(cm, "make", 2)
    tst(cm, "foo", -1)
  }
}
