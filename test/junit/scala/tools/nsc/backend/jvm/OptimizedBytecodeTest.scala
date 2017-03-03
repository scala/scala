package scala.tools.nsc.backend.jvm

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class OptimizedBytecodeTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:classpath -opt-warnings"
  import compiler._

  @Test
  def t2171(): Unit = {
    val code =
      """class C {
        |  final def m(msg: => String) = try 0 catch { case ex: Throwable => println(msg) }
        |  def t(): Unit = while (true) m("...")
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t"), List(Label(0), Jump(GOTO, Label(0))))
  }

  @Test
  def t3430(): Unit = {
    val code =
      """class C {
        |  final def m(f: String => Boolean) = f("a")
        |  def t(): Boolean =
        |    m { s1 =>
        |      m { s2 =>
        |        while (true) { }
        |        true
        |      }
        |    }
        |}
      """.stripMargin
    val c = compileClass(code)

    assertSameSummary(getMethod(c, "t"), List(
      LDC, ASTORE, ALOAD /*0*/, ALOAD /*1*/, "$anonfun$t$1", IRETURN))
    assertSameSummary(getMethod(c, "$anonfun$t$1"), List(LDC, "$anonfun$t$2", IRETURN))
    assertSameSummary(getMethod(c, "$anonfun$t$2"), List(-1 /*A*/, GOTO /*A*/))
  }

  @Test
  def t3252(): Unit = {
    val code =
      """class C {
        |  def t(x: Boolean): Thread = {
        |    g {
        |      x match {
        |        case false => Tat.h { }
        |      }
        |    }
        |  }
        |
        |  private def g[T](block: => T) = ???
        |}
        |object Tat {
        |  def h(block: => Unit): Nothing = ???
        |}
      """.stripMargin
    val List(c, t, tMod) = compileClasses(code, allowMessage = _.msg.contains("not be exhaustive"))
    assertSameSummary(getMethod(c, "t"), List(GETSTATIC, "$qmark$qmark$qmark", ATHROW))
  }

  @Test
  def t6157(): Unit = {
    val code =
      """class C {
        |  def t = println(ErrorHandler.defaultIfIOException("String")("String"))
        |}
        |object ErrorHandler {
        |  import java.io.IOException
        |  @inline
        |  def defaultIfIOException[T](default: => T)(closure: => T): T = try closure catch {
        |    case e: IOException => default
        |  }
        |}
      """.stripMargin

    val msg =
      """ErrorHandler$::defaultIfIOException(Lscala/Function0;Lscala/Function0;)Ljava/lang/Object; is annotated @inline but could not be inlined:
        |The operand stack at the callsite in C::t()V contains more values than the
        |arguments expected by the callee ErrorHandler$::defaultIfIOException(Lscala/Function0;Lscala/Function0;)Ljava/lang/Object;. These values would be discarded
        |when entering an exception handler declared in the inlined method.""".stripMargin

    compileClasses(code, allowMessage = _.msg == msg)
  }

  @Test
  def t6547(): Unit = { // "pos" test -- check that it compiles
    val code =
      """trait ConfigurableDefault[@specialized V] {
        |  def fillArray(arr: Array[V], v: V) = (arr: Any) match {
        |    case x: Array[Int]  => null
        |    case x: Array[Long] => v.asInstanceOf[Long]
        |  }
        |}
      """.stripMargin
    compileToBytes(code)
  }

  @Test
  def t8062(): Unit = {
    val c1 =
      """package warmup
        |object Warmup { def filter[A](p: Any => Boolean): Any = filter[Any](p) }
      """.stripMargin
    val c2 = "class C { def t = warmup.Warmup.filter[Any](x => false) }"
    val List(c, _, _) = compileClassesSeparately(List(c1, c2), extraArgs = compilerArgs)
    assertInvoke(getMethod(c, "t"), "warmup/Warmup$", "filter")
  }

  @Test
  def t8306(): Unit = { // "pos" test
    val code =
      """class C {
        |  def foo: Int = 123
        |  lazy val extension: Int = foo match {
        |    case idx if idx != -1 => 15
        |    case _ => 17
        |  }
        |}
      """.stripMargin
    compileToBytes(code)
  }

  @Test
  def t8359(): Unit = { // "pos" test
    // This is a minimization of code that crashed the compiler during bootstrapping
    // in the first iteration of https://github.com/scala/scala/pull/4373, the PR
    // that adjusted the order of free and declared params in LambdaLift.

    // Was:
    //  java.lang.AssertionError: assertion failed:
    //  Record Record(<$anon: Function1>,Map(value a$1 -> Deref(LocalVar(value b)))) does not contain a field value b$1
    // at scala.tools.nsc.Global.assert(Global.scala:262)
    // at scala.tools.nsc.backend.icode.analysis.CopyPropagation$copyLattice$State.getFieldNonRecordValue(CopyPropagation.scala:113)
    // at scala.tools.nsc.backend.icode.analysis.CopyPropagation$copyLattice$State.getFieldNonRecordValue(CopyPropagation.scala:122)
    // at scala.tools.nsc.backend.opt.ClosureElimination$ClosureElim$$anonfun$analyzeMethod$1$$anonfun$apply$2.replaceFieldAccess$1(ClosureElimination.scala:124)
    val code =
      """package test
        |class Typer {
        |  def bar(a: Boolean, b: Boolean): Unit = {
        |    @inline
        |    def baz(): Unit = {
        |      ((_: Any) => (Typer.this, a, b)).apply("")
        |    }
        |    ((_: Any) => baz()).apply("")
        |  }
        |}
      """.stripMargin
    compileToBytes(code)
  }

  @Test
  def t9123(): Unit = { // "pos" test
    val code =
      """trait Setting {
        |  type T
        |  def value: T
        |}
        |object Test {
        |  def test(x: Some[Setting]) = x match {
        |    case Some(dep) => Some(dep.value) map (_ => true)
        |  }
        |}
      """.stripMargin
    compileToBytes(code)
  }

  @Test
  def traitForceInfo(): Unit = {
    // This did NOT crash unless it's in the interactive package.
    // error: java.lang.AssertionError: assertion failed: trait Contexts.NoContext$ linkedModule: <none>List()
    //  at scala.Predef$.assert(Predef.scala:160)
    //  at scala.tools.nsc.symtab.classfile.ClassfileParser$innerClasses$.innerSymbol$1(ClassfileParser.scala:1211)
    //  at scala.tools.nsc.symtab.classfile.ClassfileParser$innerClasses$.classSymbol(ClassfileParser.scala:1223)
    //  at scala.tools.nsc.symtab.classfile.ClassfileParser.classNameToSymbol(ClassfileParser.scala:489)
    //  at scala.tools.nsc.symtab.classfile.ClassfileParser.sig2type$1(ClassfileParser.scala:757)
    //  at scala.tools.nsc.symtab.classfile.ClassfileParser.sig2type$1(ClassfileParser.scala:789)
    val code =
      """package scala.tools.nsc
        |package interactive
        |
        |trait MyContextTrees {
        |  val self: Global
        |  val NoContext = self.analyzer.NoContext
        |}
      """.stripMargin
    compileClasses(code)
  }

  @Test
  def t9160(): Unit = {
    val code =
      """class C {
        |  def getInt: Int = 0
        |  def t(trees: Object): Int = {
        |    trees match {
        |      case Some(elems) =>
        |      case tree => getInt
        |    }
        |    55
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t"), List(
        ALOAD /*1*/, INSTANCEOF /*Some*/, IFNE /*A*/,
        ALOAD /*0*/, "getInt", POP,
        -1 /*A*/, BIPUSH, IRETURN))
  }

  @Test
  def t8796(): Unit = {
    val code =
      """final class C {
        |  def pr(): Unit = ()
        |  def t(index: Int): Unit = index match {
        |    case 0 => pr()
        |    case 1 => pr()
        |    case _ => t(index - 2)
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t"), List(
      -1 /*A*/, ILOAD /*1*/, TABLESWITCH,
      -1, ALOAD, "pr", RETURN,
      -1, ALOAD, "pr", RETURN,
      -1, ILOAD, ICONST_2, ISUB, ISTORE, GOTO /*A*/))
  }

  @Test
  def t8524(): Unit = {
    val c1 =
      """package library
        |object Library {
        |  @inline def pleaseInlineMe() = 1
        |  object Nested { @inline def pleaseInlineMe() = 2 }
        |}
      """.stripMargin

    val c2 =
      """class C {
        |  def t = library.Library.pleaseInlineMe() + library.Library.Nested.pleaseInlineMe()
        |}
      """.stripMargin

    val cls = compileClassesSeparately(List(c1, c2), extraArgs = compilerArgs)
    val c = findClass(cls, "C")
    assertSameSummary(getMethod(c, "t"), List(
      GETSTATIC, IFNONNULL, ACONST_NULL, ATHROW, // module load and null checks not yet eliminated
      -1, ICONST_1, GETSTATIC, IFNONNULL, ACONST_NULL, ATHROW,
      -1, ICONST_2, IADD, IRETURN))
  }

  @Test
  def privateInline(): Unit = {
    val code =
      """final class C {
        |  private var x1 = false
        |  var x2 = false
        |
        |  @inline private def wrapper1[T](body: => T): T = {
        |    val saved = x1
        |    x1 = true
        |    try body
        |    finally x1 = saved
        |  }
        |
        |  @inline private def wrapper2[T](body: => T): T = {
        |    val saved = x2
        |    x2 = true
        |    try body
        |    finally x2 = saved
        |  }
        |   // inlined
        |  def f1a() = wrapper1(5)
        |  // not inlined: even after inlining `identity`, the Predef module is already on the stack for the
        |  // subsequent null check (the receiver of an inlined method, in this case Predef, is checked for
        |  // nullness, to ensure an NPE is thrown)
        |  def f1b() = identity(wrapper1(5))
        |
        |  def f2a() = wrapper2(5)            // inlined
        |  def f2b() = identity(wrapper2(5))  // not inlined
        |}
      """.stripMargin
    val c = compileClass(code, allowMessage = _.msg.contains("exception handler declared in the inlined method"))
    assertInvoke(getMethod(c, "f1a"), "C", "$anonfun$f1a$1")
    assertInvoke(getMethod(c, "f1b"), "C", "wrapper1")
    assertInvoke(getMethod(c, "f2a"), "C", "$anonfun$f2a$1")
    assertInvoke(getMethod(c, "f2b"), "C", "wrapper2")
  }

  @Test
  def t7060(): Unit = {
    val code =
      """class C {
        |  @inline final def mbarray_apply_minibox(array: Any, tag: Byte): Long =
        |    if (tag == 0) array.asInstanceOf[Array[Long]](0)
        |    else array.asInstanceOf[Array[Byte]](0).toLong
        |
        |  def t = mbarray_apply_minibox(null, 0)
        |}
      """.stripMargin
    val c = compileClass(code)
    assertNoInvoke(getMethod(c, "t"))
  }

  @Test
  def t8315(): Unit = {
    val code =
      """class C {
        |  def t(as: Listt): Unit = {
        |    map(as, (_: Any) => return)
        |  }
        |  final def map(x: Listt, f: Any => Any): Any = {
        |    if (x eq Nill) "" else f("")
        |  }
        |}
        |object Nill extends Listt
        |class Listt
      """.stripMargin
    val List(c, nil, nilMod, listt) = compileClasses(code)
    assertInvoke(getMethod(c, "t"), "C", "$anonfun$t$1")
  }

  @Test
  def t8315b(): Unit = {
    val code =
      """class C {
        |  def crash: Unit = {
        |    val key = ""
        |    try map(new F(key))
        |    catch { case _: Throwable => }
        |  }
        |  final def map(f: F): Any = f.apply("")
        |}
        |final class F(key: String) {
        |  final def apply(a: Any): Any = throw new RuntimeException(key)
        |}
      """.stripMargin
    val List(c, f) = compileClasses(code)
    assertInvoke(getMethod(c, "crash"), "C", "map")
  }

  @Test
  def optimiseEnablesNewOpt(): Unit = {
    val code = """class C { def t = (1 to 10) foreach println }"""
    val List(c) = readAsmClasses(newCompiler(extraArgs = "-optimise -deprecation").compileToBytes(code, allowMessage = _.msg.contains("is deprecated")))
    assertInvoke(getMethod(c, "t"), "C", "$anonfun$t$1") // range-foreach inlined from classpath
  }
}
