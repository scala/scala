package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

/**
  * Tests for boxing/unboxing optimizations.
  */
@RunWith(classOf[JUnit4])
class BoxUnboxTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:method"
  import compiler._

  @Test
  def elimUnusedBoxUnbox(): Unit = {
    val code =
      """class C {
        |  def t(a: Long): Int = {
        |    val t = 3 + a
        |    val u = a + t
        |    val v: Any = u // scala/runtime/BoxesRunTime.boxToLong
        |
        |    val w = (v, a) // a Tuple2 (not specialized because first value is Any)
        |                   // so calls scala/runtime/BoxesRunTime.boxToLong on the second value
        |
        |    val x = v.asInstanceOf[Long] // scala/runtime/BoxesRunTime.unboxToLong
        |
        |    val z = (java.lang.Long.valueOf(a), t)  // java box call on the left, scala/runtime/BoxesRunTime.boxToLong on the right
        |
        |    0
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t"), List(Op(ICONST_0), Op(IRETURN)))
  }

  @Test
  def boxUnboxPrimitive(): Unit = {
    val code =
      """class C {
        |  def t1 = {
        |    val a: Any = runtime.BoxesRunTime.boxToInteger(1)
        |    runtime.BoxesRunTime.unboxToInt(a) + 1
        |  }
        |
        |  // two box and two unbox operations
        |  def t2(b: Boolean) = {
        |    val a = if (b) (3l: Any) else 2l
        |    a.asInstanceOf[Long] + 1 + a.asInstanceOf[Long]
        |  }
        |
        |  def t3(i: Integer): Int = i.asInstanceOf[Int]
        |
        |  def t4(l: Long): Any = l
        |
        |  def t5(i: Int): Int = {
        |    val b = Integer.valueOf(i)
        |    val c: Integer = i
        |    b.asInstanceOf[Int] + c.intValue
        |  }
        |
        |  def t6: Long = {
        |    val y = new java.lang.Boolean(true)
        |    val i: Integer = if (y) new Integer(10) else 13
        |    val j: java.lang.Long = 3l
        |    j + i
        |  }
        |
        |  def t7: Int = {
        |    val a: Any = 3
        |    a.asInstanceOf[Int] + a.asInstanceOf[Int]
        |  }
        |
        |  def t8 = null.asInstanceOf[Int]
        |
        |  def t9: Int = {
        |    val a = Integer.valueOf(10)
        |    val b = runtime.BoxesRunTime.unboxToInt(a)
        |    a + b
        |  }
        |
        |  @noinline def escape(a: Any) = ()
        |
        |  // example E4 in BoxUnbox doc comment
        |  def t10: Int = {
        |    val a = Integer.valueOf(10) // int 10 is stored into local
        |    escape(a)
        |    a                           // no unbox, 10 is read from local
        |  }
        |
        |  // the boxes here cannot be eliminated. see doc comment in BoxUnbox, example E1.
        |  def t11(b: Boolean): Int = {
        |    val i = Integer.valueOf(10)
        |    val j = Integer.valueOf(41)
        |    escape(i)                          // force rewrite method M1 (see doc in BoxUnbox)
        |    val res: Integer = if (b) i else j
        |    res.toInt                          // cannot be re-written to a local variable read - we don't know which local to read
        |  }
        |
        |  // both boxes have a single unboxing consumer, and the escape. note that the escape does
        |  // NOT put the two boxes into the same set of rewrite operations: we can rewrite both
        |  // boxes with their unbox individually. in both cases the box also escapes, so method
        |  // M1 will keep the box around.
        |  def t12(b: Boolean): Int = {
        |    val i = Integer.valueOf(10)
        |    val j = Integer.valueOf(32)
        |    escape(if (b) i else j)      // force method M1. the escape here is a consumer for both boxes
        |    if (b) i.toInt else j.toInt  // both boxes (i, j) have their own unboxing consumer
        |  }
        |
        |  def t13(i: Int, j: Int): Int = (i: Any, j: Any) match { // used to be boxToInteger x2
        |    case (a: Int, b: Int) => a + b
        |    case _                => -1
        |  }
        |
        |  // we need to make sure that since x and y escape, we don't accidentally forget to deref them
        |  def t14(i: Int, j: Int)(b: Boolean): Int = {
        |    var (x: Int, y: Int) = (i, j)
        |    val close = (c: Boolean, n: Int) => if (c) x += n else y += n
        |    escape(close)
        |    (x, y) match {
        |      case (w: Int, z: Int) => w + z
        |      case _ => 0
        |    }
        |  }
        |
        |  def t15(i: Int): (Boolean, Boolean) = {
        |    val boxt = (i: Integer)
        |    (boxt.isInstanceOf[Object], boxt.isInstanceOf[Number])
        |  }
        |
        |  def t16(i: Int, l: Long) = {
        |    val bi: java.lang.Integer = i
        |    val li: java.lang.Long = l
        |    bi + li
        |  }
        |
        |}
      """.stripMargin

    val c = compileClass(code)

    assertNoInvoke(getMethod(c, "t1"))
    assertNoInvoke(getMethod(c, "t2"))
    assertInvoke(getMethod(c, "t3"), "scala/runtime/BoxesRunTime", "unboxToInt")
    assertInvoke(getMethod(c, "t4"), "scala/runtime/BoxesRunTime", "boxToLong")
    assertNoInvoke(getMethod(c, "t5"))
    assertNoInvoke(getMethod(c, "t6"))
    assertNoInvoke(getMethod(c, "t7"))
    assertSameSummary(getMethod(c, "t8"), List(ICONST_0, IRETURN))
    assertNoInvoke(getMethod(c, "t9"))
    // t10: no invocation of unbox
    assertEquals(getInstructions(c, "t10") collect { case Invoke(_, owner, name, _, _) => (owner, name) }, List(
      ("java/lang/Integer", "valueOf"),
      ("C", "escape")))

    assertSameSummary(getMethod(c, "t11"), List(
      BIPUSH, "valueOf", ASTORE /*2*/ ,
      BIPUSH, "valueOf", ASTORE /*3*/ ,
      ALOAD /*0*/ , ALOAD /*2*/ , "escape",
      ILOAD /*1*/ , IFEQ /*L1*/ , ALOAD /*2*/ , GOTO /*L2*/ , /*Label L1*/ -1, ALOAD /*3*/ , /*Label L2*/ -1,
      ASTORE /*4*/ , GETSTATIC /*Predef*/ , ALOAD /*4*/ , "Integer2int", IRETURN))

    // no unbox invocations
    assertEquals(getInstructions(c, "t12") collect { case Invoke(_, owner, name, _, _) => (owner, name) }, List(
      ("java/lang/Integer", "valueOf"),
      ("java/lang/Integer", "valueOf"),
      ("C", "escape")))

    assertNoInvoke(getMethod(c, "t13"))
    assertSameSummary(getInstructions(c, "t13"), List(ILOAD /*1*/, ILOAD /*2*/, IADD, IRETURN))

    assertEquals(getInstructions(c, "t14") collect { case Invoke(_, owner, name, _, _) => (owner, name) }, List(
      ("scala/runtime/IntRef", "create"),
      ("scala/runtime/IntRef", "create"),
      ("C", "escape")
    ))

    assertEquals(getInstructions(c, "t14") collect { case Field(_, owner, name, _) => (owner, name) }, List(
      ("scala/runtime/IntRef", "elem"),
      ("scala/runtime/IntRef", "elem")
    ))

    assertDoesNotInvoke(getInstructions(c, "t15"), "boxToInteger")
    assertSameSummary(getMethod(c, "t15"), List(NEW, DUP, ICONST_1, ICONST_1, "<init>", ARETURN))

    assertDoesNotInvoke(getInstructions(c, "t16"), "boxToInteger")
    assertDoesNotInvoke(getInstructions(c, "t16"), "boxToLong")
    assertDoesNotInvoke(getInstructions(c, "t16"), "unboxToInt")
    assertDoesNotInvoke(getInstructions(c, "t16"), "unboxToLong")
  }

  @Test
  def refEliminiation(): Unit = {
    val code =
      """class C {
        |  import runtime._
        |  @noinline def escape(a: Any) = ()
        |
        |  def t1 = { // box eliminated
        |    val r = new IntRef(0)
        |    r.elem
        |  }
        |
        |  def t2(b: Boolean) = {
        |    val r1 = IntRef.zero()    // both eliminated
        |    val r2 = IntRef.create(1)
        |    val res: IntRef = if (b) r1 else r2
        |    res.elem
        |  }
        |
        |  def t3 = {
        |    val r = LongRef.create(10l) // eliminated
        |    r.elem += 3
        |    r.elem
        |  }
        |
        |  def t4(b: Boolean) = {
        |    val x = BooleanRef.create(false) // eliminated
        |    if (b) x.elem = true
        |    if (x.elem) "a" else "b"
        |  }
        |
        |  def t5 = {
        |    val r = IntRef.create(10) // not eliminated: the box might be modified in the escape
        |    escape(r)
        |    r.elem
        |  }
        |
        |  def t6(b: Boolean) = {
        |    val r1 = IntRef.zero()
        |    val r2 = IntRef.create(1)
        |    r1.elem = 39
        |    val res: IntRef = if (b) r1 else r2
        |    res.elem // boxes remain: can't rewrite this read, don't know which local
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t1"), List(ICONST_0, IRETURN))
    assertNoInvoke(getMethod(c, "t2"))
    assertSameSummary(getMethod(c, "t3"), List(LDC, LDC, LADD, LRETURN))
    assertNoInvoke(getMethod(c, "t4"))
    assertEquals(getInstructions(c, "t5") collect { case Field(_, owner, name, _) => s"$owner.$name" },
      List("scala/runtime/IntRef.elem"))
    assertEquals(getInstructions(c, "t6") collect { case Field(op, owner, name, _) => s"$op $owner.$name" },
      List(s"$PUTFIELD scala/runtime/IntRef.elem", s"$GETFIELD scala/runtime/IntRef.elem"))
  }

  @Test
  def tupleElimination(): Unit = {
    val code =
      """class C {
        |  def t1(b: Boolean) = {
        |    val t = ("hi", "fish")
        |    if (b) t._1 else t._2
        |  }
        |
        |  def t2 = {
        |    val t = (1, 3) // specialized tuple
        |    t._1 + t._2    // specialized accessors (_1$mcII$sp)
        |  }
        |
        |  def t3 = {
        |    // boxed before tuple creation, a non-specialized tuple is created
        |    val t = (new Integer(3), Integer.valueOf(4))
        |    t._1 + t._2 // invokes the generic `_1` / `_2` getters, both values unboxed by Integer2int
        |  }
        |
        |  def t4: Any = {
        |    val t = (3, 3) // specialized tuple is created, ints are not boxed
        |    (t: Tuple2[Any, Any])._1 // when eliminating the _1 call, need to insert a boxing operation
        |  }
        |
        |  // the inverse of t4 also happens: an Tuple[Integer] where _1$mcI$sp is invoked. In this
        |  // case, an unbox operation needs to be added when eliminating the extraction. The only
        |  // way I found to test this is with an inlined generic method, see InlinerTest.tupleElimination.
        |  def tpl[A, B](a: A, b: B) = (a, b)
        |  def t5: Int = tpl(1, 2)._1 // invokes _1$mcI$sp
        |
        |  def t6 = {
        |    val (a, b) = (1, 2)
        |    a - b
        |  }
        |
        |  def t7 = {
        |    // this example is more tricky to handle than it looks, see doc comment in BoxUnbox.
        |    val ((a, b), c) = ((1, 2), 3)
        |    a + b + c
        |  }
        |
        |  def t8 = {
        |    val ((a, b), (c, d)) = ((1, 2), (3, Integer.valueOf(10)))
        |    a + b + c + d
        |  }
        |
        |  def t9(a: Int, b: Int) = (a, b) match { // tuple is optimized away
        |    case (x, y) if x == y => 0
        |    case (x, y) => x + y
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertNoInvoke(getMethod(c, "t1"))
    assertSameSummary(getMethod(c, "t2"), List(ICONST_1, ICONST_3, IADD, IRETURN))
    assertSameSummary(getMethod(c, "t3"), List(ICONST_3, ICONST_4, IADD, IRETURN))
    assertSameSummary(getMethod(c, "t4"), List(ICONST_3, "boxToInteger", ARETURN))
    assertEquals(getInstructions(c, "t5") collect { case Invoke(_, owner, name, _, _) => (owner, name) }, List(
      ("scala/runtime/BoxesRunTime", "boxToInteger"),
      ("scala/runtime/BoxesRunTime", "boxToInteger"),
      ("C", "tpl"),
      ("scala/Tuple2", "_1$mcI$sp")))
    assertSameSummary(getMethod(c, "t6"), List(ICONST_1, ICONST_2, ISUB, IRETURN))
    assertSameSummary(getMethod(c, "t7"), List(
      ICONST_1, ICONST_2, ISTORE, ISTORE,
      ICONST_3, ISTORE,
      ILOAD, ILOAD, IADD, ILOAD, IADD, IRETURN))
    assertNoInvoke(getMethod(c, "t8"))
    assertNoInvoke(getMethod(c, "t9"))
  }

}
