package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable.ListBuffer
import scala.tools.asm.Attribute
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree.{ClassNode, InnerClassNode}
import scala.tools.nsc.symtab.classfile.JavaSignatureWalker
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class BytecodeTest extends BytecodeTesting {
  import compiler._

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
      Op(ICONST_1), Jump(GOTO, Label(9)),
      Label(6), Op(ICONST_2),
      Label(9), Op(IRETURN)))

    // t2: no unnecessary GOTOs
    assertSameCode(getMethod(c, "t2"), List(
      VarOp(ILOAD, 1), IntOp(SIPUSH, 393), Jump(IF_ICMPNE, Label(7)),
      Op(ICONST_1), Jump(GOTO, Label(10)),
      Label(7), Op(ICONST_2),
      Label(10), Op(IRETURN)))

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
      Label(12), Op(ICONST_1), Jump(GOTO, Label(19)),
      Label(16), Op(ICONST_2),
      Label(19), Op(IRETURN)))

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
      Label(0), Ldc(LDC, ""), Label(3), VarOp(ASTORE, 1),
      Label(5), VarOp(ALOAD, 1), Jump(IFNULL, Label(21)),
      Label(10), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "foo", "()V", false), Label(14), Op(ACONST_NULL), VarOp(ASTORE, 1), Label(18), Jump(GOTO, Label(5)),
      Label(21), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "bar", "()V", false), Label(26), Op(RETURN), Label(28)))
    val labels = t.instructions collect { case l: Label => l }
    val x = t.localVars.find(_.name == "x").get
    assertEquals(x.start, labels(1))
    assertEquals(x.end, labels(7))
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
  def t10180: Unit = {
    // Inner classes referenced only in generic signatures should contribute to the
    // InnerClass attribute.
    val code =
      """abstract class E { def foo: Option[C#D] }
        class F { private[this] val foo: Option[C#D] = null }
        abstract class G extends Base[C#D]
        abstract class H[T <: Base[C#D]]
        abstract class I { def foo[T <: Base[C#D]] = 42 }
        abstract class J { def foo[T <: Base[Array[C#D]]] = 42 }
        class C {
          class D
        }
        class Base[T]
      """.stripMargin
    val classes = compileClasses(code)
    def check(className: String): Unit = {
      val inners = classes.find(_.name == className).get.innerClasses
      assertTrue(inners.size == 1)
      assertEquals("D", inners.get(0).innerName)
      assertEquals("C", inners.get(0).outerName)
    }
    check("E")
    check("F")
    check("G")
    check("H")
    check("I")
    check("J")
  }

  @Test
  def t10180_walker: Unit = {
    def namesOf(sig: String): List[String] = {
      val buffer = ListBuffer[String]()
      val walker = new JavaSignatureWalker {
        override def visitName(internalName: CharSequence): Unit = buffer += internalName.toString
        override def raiseError(msg: String): Unit = throw new RuntimeException(msg)
      }
      walker.walk(sig)
      buffer.toList
    }
    assertEquals(List("a/b/C"), namesOf("La/b/C;"))
    assertEquals(List("a/b/C"), namesOf("[[La/b/C;"))
    assertEquals(List("a/b/C"), namesOf("()La/b/C;"))
    assertEquals(List("X", "Z", "Z", "a/b/C$D"), namesOf("()La/b/C<LX;>.D<+LZ;-LZ;>;"))
    assertEquals(List("a/b/C"), namesOf("()La/b/C<*>;"))
    assertEquals(List("java/lang/String", "java/lang/Cloneable", "java/io/Serializable", "java/io/Serializable", "java/lang/Object"), namesOf("<T:Ljava/lang/String;:Ljava/lang/Cloneable;:Ljava/io/Serializable;U::Ljava/io/Serializable;>Ljava/lang/Object;"))
    // Currently, scalac neither consumes generic throws declarations from signatures in ClassFileParser, nor does it emit
    // them in `javaSig`. I've future proofed JavaSignatureWalker to parse them if that changes.
    assertEquals(List("T1", "T2"), namesOf("()V^LT1;^LT2;"))
  }

  @Test
  @Ignore("manually run test")
  def walker(): Unit = {
    val walker = new JavaSignatureWalker {
      override def visitName(internalName: CharSequence): Unit = ()
      override def raiseError(msg: String): Unit = throw new RuntimeException(msg)
    }
    import java.nio.file._
    import scala.collection.JavaConverters._
    val zipfile = Paths.get("/Library/Java/JavaVirtualMachines/jdk1.8.0_112.jdk/Contents/Home/jre/lib/rt.jar")
    val fs = FileSystems.newFileSystem(zipfile, null)
    val root = fs.getRootDirectories.iterator().next()
    val contents = Files.walk(root).iterator().asScala.toList
    for (f <- contents if Files.isRegularFile(f) && f.getFileName.toString.endsWith(".class")) {
      val classNode = AsmUtils.classFromBytes(Files.readAllBytes(f))
      def signaturesOf(cls: ClassNode): Iterator[String] = {
        val sigs = Iterator(classNode.signature) ++ classNode.methods.iterator().asScala.map(_.signature) ++ classNode.fields.iterator().asScala.map(_.signature)
        sigs.filter(_ != null)
      }
      for (sig <- signaturesOf(classNode)) {
        walker.walk(sig)
      }
    }

  }
}
