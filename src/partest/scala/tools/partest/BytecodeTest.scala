package scala.tools.partest

import scala.tools.nsc.util.JavaClassPath
import scala.collection.JavaConverters._
import scala.tools.asm
import asm.ClassReader
import asm.tree.{ClassNode, MethodNode, InsnList}
import java.io.InputStream

/**
 * Provides utilities for inspecting bytecode using ASM library.
 *
 * HOW TO USE
 * 1. Create subdirectory in test/files/jvm for your test. Let's name it $TESTDIR.
 * 2. Create $TESTDIR/BytecodeSrc_1.scala that contains Scala source file that you
 *    want to inspect the bytecode for. The '_1' suffix signals to partest that it
 *    should compile this file first.
 * 3. Create $TESTDIR/Test.scala:
 *    import scala.tools.partest.BytecodeTest
 *    object Test extends BytecodeTest {
 *      def show {
 *        // your code that inspect ASM trees and prints values
 *      }
 *    }
 * 4. Create corresponding check file.
 *
 * EXAMPLE
 * See test/files/jvm/bytecode-test-example for an example of bytecode test.
 *
 */
abstract class BytecodeTest extends ASMConverters {

  /** produce the output to be compared against a checkfile */
  protected def show(): Unit

  def main(args: Array[String]): Unit = show

// asserts
  def sameBytecode(methA: MethodNode, methB: MethodNode) = {
    val isa = instructions.fromMethod(methA)
    val isb = instructions.fromMethod(methB)
    if (isa == isb) println("bytecode identical")
    else diffInstructions(isa, isb)
  }

  import instructions._
  // bytecode is equal modulo local variable numbering
  def equalsModuloVar(a: Instruction, b: Instruction) = (a, b) match {
    case _ if a == b => true
    case (VarOp(op1, _), VarOp(op2, _)) if op1 == op2 => true
    case _ => false
  }

  def similarBytecode(methA: MethodNode, methB: MethodNode, similar: (Instruction, Instruction) => Boolean) = {
    val isa = fromMethod(methA)
    val isb = fromMethod(methB)
    if (isa == isb) println("bytecode identical")
    else if ((isa, isb).zipped.forall { case (a, b) => similar(a, b) }) println("bytecode similar")
    else diffInstructions(isa, isb)
  }

  def diffInstructions(isa: List[Instruction], isb: List[Instruction]) = {
    val len = Math.max(isa.length, isb.length)
    if (len > 0 ) {
      val width = isa.map(_.toString.length).max
      val lineWidth = len.toString.length
      (1 to len) foreach { line =>
        val isaPadded = isa.map(_.toString) orElse Stream.continually("")
        val isbPadded = isb.map(_.toString) orElse Stream.continually("")
        val a = isaPadded(line-1)
        val b = isbPadded(line-1)

        println(s"""$line${" " * (lineWidth-line.toString.length)} ${if (a==b) "==" else "<>"} $a${" " * (width-a.length)} | $b""")
      }
    }
  }

// loading
  protected def getMethod(classNode: ClassNode, name: String): MethodNode =
    classNode.methods.asScala.find(_.name == name) getOrElse
      sys.error(s"Didn't find method '$name' in class '${classNode.name}'")

  protected def loadClassNode(name: String, skipDebugInfo: Boolean = true): ClassNode = {
    val classBytes: InputStream = (for {
      classRep <- classpath.findClass(name)
      binary <- classRep.binary
    } yield binary.input) getOrElse sys.error(s"failed to load class '$name'; classpath = $classpath")

    val cr = new ClassReader(classBytes)
    val cn = new ClassNode()
    cr.accept(cn, if (skipDebugInfo) ClassReader.SKIP_DEBUG else 0)
    cn
  }

  protected lazy val classpath: JavaClassPath = {
    import scala.tools.nsc.util.ClassPath.DefaultJavaContext
    import scala.tools.util.PathResolver.Defaults
    // logic inspired by scala.tools.util.PathResolver implementation
    val containers = DefaultJavaContext.classesInExpandedPath(Defaults.javaUserClassPath)
    new JavaClassPath(containers, DefaultJavaContext)
  }
}
