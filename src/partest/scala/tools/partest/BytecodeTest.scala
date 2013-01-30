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
abstract class BytecodeTest {

  /** produce the output to be compared against a checkfile */
  protected def show(): Unit

  def main(args: Array[String]): Unit = show

// asserts
  def sameBytecode(methA: MethodNode, methB: MethodNode) = {
    val isa = instructions.fromMethod(methA)
    val isb = instructions.fromMethod(methB)
    if (isa == isb) println("bytecode identical")
    else (isa, isb).zipped.foreach { case (a, b) =>
      if (a == b) println("OK  : "+ a)
      else println("DIFF: "+ a +" <=> "+ b)
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

  // wrap ASM's instructions so we get case class-style `equals` and `toString`
  object instructions {
    def fromMethod(meth: MethodNode): List[instructions.Instruction] = {
      val insns = meth.instructions
      val asmToScala = new AsmToScala{ def labelIndex(l: asm.tree.AbstractInsnNode) = insns.indexOf(l) }

      asmToScala.mapOver(insns.iterator.asScala.toList).asInstanceOf[List[instructions.Instruction]]
    }

    sealed abstract class Instruction { def opcode: Int }
    case class Field         (opcode: Int, desc: String, name: String, owner: String)              extends Instruction
    case class Incr          (opcode: Int, incr: Int, `var`: Int)                                  extends Instruction
    case class Op            (opcode: Int)                                                         extends Instruction
    case class IntOp         (opcode: Int, operand: Int)                                           extends Instruction
    case class Jump          (opcode: Int, label: Label)                                           extends Instruction
    case class Ldc           (opcode: Int, cst: Any)                                               extends Instruction
    case class LookupSwitch  (opcode: Int, dflt: Label, keys: List[Integer], labels: List[Label])  extends Instruction
    case class TableSwitch   (opcode: Int, dflt: Label, max: Int, min: Int, labels: List[Label])   extends Instruction
    case class Method        (opcode: Int, desc: String, name: String, owner: String)              extends Instruction
    case class NewArray      (opcode: Int, desc: String, dims: Int)                                extends Instruction
    case class TypeOp        (opcode: Int, desc: String)                                           extends Instruction
    case class VarOp         (opcode: Int, `var`: Int)                                             extends Instruction
    case class Label         (opcode: Int, offset: Int)                                            extends Instruction
    case class FrameEntry    (opcode: Int, local: List[Any], stack: List[Any])                     extends Instruction
    case class LineNumber    (opcode: Int, line: Int, start: Label)                                extends Instruction
  }

  abstract class AsmToScala {
    import instructions._
    def labelIndex(l: asm.tree.AbstractInsnNode): Int

    def mapOver(is: List[Any]): List[Any] = is map {
      case i: asm.tree.AbstractInsnNode => apply(i)
      case x => x
    }

   def lst[T](xs: java.util.List[T]): List[T] = if (xs == null) Nil else xs.asScala.toList
   def apply(l: asm.tree.LabelNode): Label = this(l: asm.tree.AbstractInsnNode).asInstanceOf[Label]
   def apply(x: asm.tree.AbstractInsnNode): Instruction = x match {
      case i: asm.tree.FieldInsnNode          => Field        (i.getOpcode: Int, i.desc: String, i.name: String, i.owner: String)
      case i: asm.tree.IincInsnNode           => Incr         (i.getOpcode: Int, i.incr: Int, i.`var`: Int)
      case i: asm.tree.InsnNode               => Op           (i.getOpcode: Int)
      case i: asm.tree.IntInsnNode            => IntOp        (i.getOpcode: Int, i.operand: Int)
      case i: asm.tree.JumpInsnNode           => Jump         (i.getOpcode: Int, this(i.label))
      case i: asm.tree.LdcInsnNode            => Ldc          (i.getOpcode: Int, i.cst: Any)
      case i: asm.tree.LookupSwitchInsnNode   => LookupSwitch (i.getOpcode: Int, this(i.dflt), lst(i.keys), mapOver(lst(i.labels)).asInstanceOf[List[Label]])
      case i: asm.tree.TableSwitchInsnNode    => TableSwitch  (i.getOpcode: Int, this(i.dflt), i.max: Int, i.min: Int, mapOver(lst(i.labels)).asInstanceOf[List[Label]])
      case i: asm.tree.MethodInsnNode         => Method       (i.getOpcode: Int, i.desc: String, i.name: String, i.owner: String)
      case i: asm.tree.MultiANewArrayInsnNode => NewArray     (i.getOpcode: Int, i.desc: String, i.dims: Int)
      case i: asm.tree.TypeInsnNode           => TypeOp       (i.getOpcode: Int, i.desc: String)
      case i: asm.tree.VarInsnNode            => VarOp        (i.getOpcode: Int, i.`var`: Int)
      case i: asm.tree.LabelNode              => Label        (i.getOpcode: Int, labelIndex(x))
      case i: asm.tree.FrameNode              => FrameEntry   (i.getOpcode: Int, mapOver(lst(i.local)), mapOver(lst(i.stack)))
      case i: asm.tree.LineNumberNode         => LineNumber   (i.getOpcode: Int, i.line: Int, this(i.start): Label)
    }
  }
}
