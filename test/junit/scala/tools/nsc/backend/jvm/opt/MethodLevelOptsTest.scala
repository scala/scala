package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class MethodLevelOptsTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:method"
  import compiler._

  def wrapInDefault(code: Instruction*) = List(Label(0), LineNumber(1, Label(0))) ::: code.toList ::: List(Label(1))

  def locals(c: ClassNode, m: String) = getAsmMethod(c, m).localVariables.asScala.toList.map(l => (l.name, l.index)).sortBy(_._2)

  @Test
  def eliminateEmptyTry(): Unit = {
    val code = "def f = { try {} catch { case _: Throwable => 0; () }; 1 }"
    val warn = "a pure expression does nothing in statement position"
    assertSameCode(compileInstructions(code, allowMessage = _.msg contains warn), wrapInDefault(Op(ICONST_1), Op(IRETURN)))
  }

  @Test
  def eliminateLoadBoxedUnit(): Unit = {
    // the compiler inserts a boxed into the try block. it's therefore non-empty (and live) and not eliminated.
    val code = "def f = { try {} catch { case _: Throwable => 0 }; 1 }"
    val m = compileMethod(code)
    assertTrue(m.handlers.length == 0)
    assertSameCode(m, List(Op(ICONST_1), Op(IRETURN)))
  }

  @Test
  def inlineThrowInCatchNotTry(): Unit = {
    // the try block does not contain the `ATHROW` instruction, but in the catch block, `ATHROW` is inlined
    val code = "def f(e: Exception) = throw { try e catch { case _: Throwable => e } }"
    val m = compileMethod(code)
    assertHandlerLabelPostions(m.handlers.head, m.instructions, 0, 3, 5)
    assertSameCode(m.instructions,
      wrapInDefault(VarOp(ALOAD, 1), Label(3), Op(ATHROW), Label(5), FrameEntry(4, List(), List("java/lang/Throwable")), Op(POP), VarOp(ALOAD, 1), Op(ATHROW))
    )
  }

  @Test
  def inlineReturnInCatchNotTry(): Unit = {
    val code = "def f: Int = return { try 1 catch { case _: Throwable => 2 } }"
    // cannot inline the IRETURN into the try block (because RETURN may throw IllegalMonitorState)
    val m = compileMethod(code)
    assertHandlerLabelPostions(m.handlers.head, m.instructions, 0, 3, 5)
    assertSameCode(m.instructions,
      wrapInDefault(Op(ICONST_1), Label(3), Op(IRETURN), Label(5), FrameEntry(4, List(), List("java/lang/Throwable")), Op(POP), Op(ICONST_2), Op(IRETURN)))
  }

  @Test
  def simplifyJumpsInTryCatchFinally(): Unit = {
    val code =
      """def f: Int =
        |  try {
        |    return 1
        |  } catch {
        |    case _: Throwable =>
        |    return 2
        |  } finally {
        |    return 3
        |    // dead
        |    val x = try 10 catch { case _: Throwable => 11 }
        |    println(x)
        |  }
      """.stripMargin
    val m = compileMethod(code)
    assertTrue(m.handlers.isEmpty)
    assertSameCode(m, List(Op(ICONST_3), Op(IRETURN)))
  }

  @Test
  def nullStoreLoadElim(): Unit = {
    // point of this test: we have two cleanups
    //   - remove `ACONST_NULL; ASTORE x` if x is otherwise not live
    //   - remove `ASTORE x; ALOAD x` if x is otherwise not live
    // in the example below, we have `ACONST_NULL; ASTORE x; ALOAD x`. in this case the store-load
    // should be removed (even though it looks like a null-store at first).
    val code =
      """class C {
        |  def t = {
        |    val x = null
        |    x.toString
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t"), List(
      Op(ACONST_NULL), Invoke(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;", false), Op(ARETURN)))
  }

  @Test
  def deadStoreReferenceElim(): Unit = {
    val code =
      """class C {
        |  def t = {
        |    var a = "a"   // assign to non-initialized, directly removed by dead store
        |    a = "b"       // assign to initialized, replaced by null-store, which is then removed: the var is not live, the uses are null-store or store-load
        |    a = "c"
        |    a             // store-load pair will be eliminated
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(
      getMethod(c, "t"), List(Ldc(LDC, "c"), Op(ARETURN)))
  }

  @Test
  def deadStoreReferenceKeepNull(): Unit = {
    val code =
      """class C {
        |  def t = {
        |    var a = "el"  // this store is live, used in the println.
        |    println(a)
        |    a = "met"     // since it's an ASTORE to a live variable, cannot elim the store (scala/bug#5313), but store null instead.
        |                  // so we get `LDC met; POP; ACONST_NULL; ASTORE 1`. the `LDC met; POP` is eliminated by push-pop.
        |    a = "zit"     // this store is live, so we get `LDC zit; ASOTRE 1; ALOAD 1; ARETURN`.
        |                  // we cannot eliminated the store-load sequence, because the local is live (again scala/bug#5313).
        |    a
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)

    assertSameCode(getMethod(c, "t"), List(
      Ldc(LDC, "el"), VarOp(ASTORE, 1),
      Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"), VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
      Op(ACONST_NULL), VarOp(ASTORE, 1),
      Ldc(LDC, "zit"), VarOp(ASTORE, 1), VarOp(ALOAD, 1), Op(ARETURN)))
  }

  @Test
  def elimUnusedTupleObjectStringBox(): Unit = {
    val code =
      """class C {
        |  def t(x: Int, y: Int): Int = {
        |    val a = (x, y)                            // Tuple2$mcII$sp
        |    val b = (a, y)                            // Tuple2
        |    val c = (new Object, "krik", new String)  // unused java/lang/Object, java/lang/String allocation and string constant is also eliminated
        |    val d = new java.lang.Integer(x)
        |    val e = new String(new Array[Char](23))   // array allocation not eliminated, as it may throw (negative size, scala/bug#8601)
        |    val f = new scala.runtime.IntRef(11)
        |    x + y
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t"), List(
      IntOp(BIPUSH, 23), IntOp(NEWARRAY, 5), Op(POP), VarOp(ILOAD, 1), VarOp(ILOAD, 2), Op(IADD), Op(IRETURN)))
  }

  @Test
  def noElimImpureConstructor(): Unit = {
    val code =
      """class C {
        |  def t(x: Int, y: Int): Int = {
        |    val a = new java.lang.Integer("nono")
        |    x + y
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t"), List(
      TypeOp(NEW, "java/lang/Integer"), Ldc(LDC, "nono"), Invoke(INVOKESPECIAL, "java/lang/Integer", "<init>", "(Ljava/lang/String;)V", false),
      VarOp(ILOAD, 1), VarOp(ILOAD, 2), Op(IADD), Op(IRETURN)))
  }

  @Test
  def elimUnusedClosure(): Unit = {
    val code =
      """class C {
        |  def t(x: Int, y: Int): Int = {
        |    val f = (a: Int) => a + x + y
        |    val g = (b: Int) => b - x
        |    val h = (s: String) => println(s)
        |    f(30)
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t"), List(
      IntOp(BIPUSH, 30), VarOp(ISTORE, 3),  // no constant propagation, so we keep the store (and load below) of a const
      VarOp(ILOAD, 1),
      VarOp(ILOAD, 2),
      VarOp(ILOAD, 3),
      Invoke(INVOKESTATIC, "C", "$anonfun$t$1", "(III)I", false), Op(IRETURN)))
  }

  @Test
  def rewriteSpecializedClosureCall(): Unit = {
    val code =
      """class C {
        |  def t = {
        |    val f1 = (x: Int) => println(x)       // int-unit specialization
        |    val f2 = (x: Int, y: Long) => x == y  // int-long-boolean
        |    f1(1)
        |    f2(3, 4)
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    val t = getMethod(c, "t")
    assert(!t.instructions.exists(_.opcode == INVOKEDYNAMIC), t)
  }

  @Test
  def nullnessOpts(): Unit = {
    val code =
      """class C {
        |  def t1 = {
        |    val a = new C
        |    if (a == null)
        |      println() // eliminated
        |    a
        |  }
        |
        |  def t2 = null.asInstanceOf[Long] // replaced by zero value
        |
        |  def t3 = {
        |    val t = (1, 3)
        |    val a = null
        |    if (t ne a) t._1
        |    else throw new Error()
        |  }
        |
        |  def t4 = {
        |    val i = Integer.valueOf(1)
        |    val a = null
        |    if (i eq a) throw new Error()
        |    else i.toInt
        |  }
        |
        |  def t5 = {
        |    val i = runtime.DoubleRef.zero()
        |    if (i == null) throw new Error()
        |    else i.elem
        |  }
        |
        |  def t6 = {
        |    var a = null
        |    var i = null
        |    a = i // eliminated (store of null to variable that is already null)
        |    a // replaced by ACONST_NULL (load of variable that is known null)
        |  }
        |
        |  def t7 = {
        |    val a = null
        |    a.isInstanceOf[String] // eliminated, replaced by 0 (null.isInstanceOf is always false)
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t1"), List(NEW, DUP, "<init>", ARETURN))
    assertSameCode(getMethod(c, "t2"), List(Op(LCONST_0), Op(LRETURN)))
    assertSameCode(getMethod(c, "t3"), List(Op(ICONST_1), Op(IRETURN)))
    assertSameCode(getMethod(c, "t4"), List(Op(ICONST_1), Op(IRETURN)))
    assertSameCode(getMethod(c, "t5"), List(Op(DCONST_0), Op(DRETURN)))
    assertSameCode(getMethod(c, "t6"), List(Op(ACONST_NULL), Op(ARETURN)))
    assertSameCode(getMethod(c, "t7"), List(Op(ICONST_0), Op(IRETURN)))
  }

  @Test
  def elimRedundantNullCheck(): Unit = {
    val code =
      """class C {
        |  def t(x: Object) = {
        |    val bool = x == null
        |    if (x != null) 1 else 0
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(
      getMethod(c, "t"), List(
        VarOp(ALOAD, 1), Jump(IFNULL, Label(6)), Op(ICONST_1), Op(IRETURN), Label(6), Op(ICONST_0), Op(IRETURN)))
  }

  @Test
  def t5313(): Unit = {
    val code =
      """class C {
        |  def randomBoolean = scala.util.Random.nextInt % 2 == 0
        |
        |  // 3 stores to kept1 (slot 1), 1 store to result (slot 2)
        |  def t1 = {
        |    var kept1 = new Object
        |    val result = new java.lang.ref.WeakReference(kept1)
        |    kept1 = null // we can't eliminate this assignment because result can observe
        |                 // when the object has no more references. See scala/bug#5313
        |    kept1 = new Object // could eliminate this one with a more elaborate analysis (we know it contains null)
        |                       // however, such is not implemented: if a var is live, then stores are kept.
        |    result
        |  }
        |
        |  // only two variables are live: kept2 and kept3. they end up on slots 1 and 2.
        |  // kept2 has 2 stores, kept3 has 1 store.
        |  def t2 = {
        |    var erased2 = null    // we can eliminate this store because it's never used
        |    val erased3 = erased2 // and this
        |    var erased4 = erased2 // and this
        |    val erased5 = erased4 // and this
        |    var kept2: Object = new Object // ultimately can't be eliminated
        |    while(randomBoolean) {
        |      val kept3 = kept2
        |      kept2 = null // this can't, because it clobbers kept2, which is used
        |      erased4 = null // safe to eliminate
        |      println(kept3)
        |    }
        |    0
        |  }
        |
        |  def t3 = {
        |    var kept4 = new Object // have to keep, it's used
        |    try
        |      println(kept4)
        |    catch {
        |      case _ : Throwable => kept4 = null // have to keep, it clobbers kept4 which is used
        |    }
        |    0
        |  }
        |
        |  def t4 = {
        |    var kept5 = new Object
        |    print(kept5)
        |    kept5 = null // can't eliminate it's a clobber and it's used
        |    print(kept5)
        |    kept5 = null // eliminated by nullness analysis (store null to a local that is known to be null)
        |    0
        |  }
        |
        |  def t5 = {
        |    while(randomBoolean) {
        |      var kept6: AnyRef = null // not used, but have to keep because it clobbers the next used store
        |                               // on the back edge of the loop
        |      kept6 = new Object // used
        |      println(kept6)
        |    }
        |    0
        |  }
        |}
      """.stripMargin

    val c = compileClass(code)
    def stores(m: String) = getInstructions(c, m).filter(_.opcode == ASTORE)

    assertEquals(locals(c, "t1"), List(("this",0), ("kept1",1), ("result",2)))
    assert(stores("t1") == List(VarOp(ASTORE, 1), VarOp(ASTORE, 2), VarOp(ASTORE, 1), VarOp(ASTORE, 1)),
      textify(getAsmMethod(c, "t1")))

    assertEquals(locals(c, "t2"), List(("this",0), ("kept2",1), ("kept3",2)))
    assert(stores("t2") == List(VarOp(ASTORE, 1), VarOp(ASTORE, 2), VarOp(ASTORE, 1)),
      textify(getAsmMethod(c, "t2")))

    assertEquals(locals(c, "t3"), List(("this",0), ("kept4",1)))
    assert(stores("t3") == List(VarOp(ASTORE, 1), VarOp(ASTORE, 1)),
      textify(getAsmMethod(c, "t3")))

    assertEquals(locals(c, "t4"), List(("this",0), ("kept5",1)))
    assert(stores("t4") == List(VarOp(ASTORE, 1), VarOp(ASTORE, 1)),
      textify(getAsmMethod(c, "t4")))

    assertEquals(locals(c, "t5"), List(("this",0), ("kept6",1)))
    assert(stores("t5") == List(VarOp(ASTORE, 1), VarOp(ASTORE, 1)),
      textify(getAsmMethod(c, "t5")))
  }

  @Test
  def testCpp(): Unit = {
    // copied from an old test (run/test-cpp.scala)
    val code =
      """class C {
        |  import scala.util.Random._
        |
        |  def t1(x: Int) = {
        |    val y = x
        |    println(y)
        |  }
        |
        |  def t2 = {
        |    val x = 2
        |    val y = x
        |    println(y)
        |  }
        |
        |  def t3 = {
        |    val x = this
        |    val y = x
        |    println(y)
        |  }
        |
        |  def f = nextInt
        |
        |  def t4 = {
        |    val x = f
        |    val y = x
        |    println(y)
        |  }
        |
        |  def t5 = {
        |    var x = nextInt
        |    var y = x
        |    println(y)
        |
        |    y = nextInt
        |    x = y
        |    println(x)
        |  }
        |}
      """.stripMargin

    val c = compileClass(code)
    assertEquals(locals(c, "t1"), List(("this", 0), ("x", 1)))

    assertEquals(locals(c, "t2"), List(("this", 0), ("x", 1)))
    // we don't have constant propagation (yet).
    // the local var can't be optimized as a store;laod sequence, there's a GETSTATIC between the two
    assertSameSummary(getMethod(c, "t2"), List(
      ICONST_2, ISTORE, GETSTATIC, ILOAD, "boxToInteger", "println", RETURN))

    assertEquals(locals(c, "t3"), List(("this", 0)))
    assertEquals(locals(c, "t4"), List(("this", 0), ("x", 1)))
    assertEquals(locals(c, "t5"), List(("this", 0), ("x", 1)))
  }

  @Test
  def t7006(): Unit = {
    val code =
      """class C {
        |  def t: Unit = {
        |    try {
        |      val x = 3
        |    } finally {
        |      print("hello")
        |    }
        |    while(true) { }
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    val t = getMethod(c, "t")
    assertEquals(t.handlers, Nil)
    assertEquals(locals(c, "t"), List(("this", 0)))
    assertSameSummary(t, List(GETSTATIC, LDC, "print", -1, GOTO))
  }

  @Test
  def booleanOrderingCompare(): Unit = {
    val code =
      """class C {
        |  def compare(x: Boolean, y: Boolean) = (x, y) match {
        |    case (false, true) => -1
        |    case (true, false) => 1
        |    case _ => 0
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertNoInvoke(getMethod(c, "compare"))
  }

  @Test
  def t8790(): Unit = {
    val code =
      """class C {
        |  def t(x: Int, y: Int): String = (x, y) match {
        |    case (7, 8) => "a"
        |    case _ => "b"
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)

    assertSameSummary(getMethod(c, "t"), List(
      BIPUSH, ILOAD, IF_ICMPNE,
      BIPUSH, ILOAD, IF_ICMPNE,
      LDC, ASTORE, GOTO,
      -1, LDC, ASTORE,
      -1, ALOAD, ARETURN))
  }

  @Test
  def elimSamLambda(): Unit = {
    val code =
      """class C {
        |  def t1(x: Int) = {
        |    val fun: java.util.function.IntFunction[Int] = y => y + 1
        |    fun(x)
        |  }
        |  def t2(x: Int) = {
        |    val fun: T = i => i + 1
        |    fun.f(x)
        |  }
        |}
        |trait T { def f(x: Int): Int }
      """.stripMargin
    val List(c, t) = compileClasses(code)
    assertSameSummary(getMethod(c, "t1"), List(ILOAD, "$anonfun$t1$1", IRETURN))
    assertSameSummary(getMethod(c, "t2"), List(ILOAD, "$anonfun$t2$1", IRETURN))
  }
}
