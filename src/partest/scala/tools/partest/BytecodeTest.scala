package scala.tools.partest

import scala.tools.nsc.util.JavaClassPath
import scala.collection.JavaConverters._
import scala.tools.asm
import asm.ClassReader
import asm.tree.{ClassNode, MethodNode, InsnList}
import java.io.InputStream

/**
 * Providies utilities for inspecting bytecode using ASM library.
 *
 * HOW TO USE
 * 1. Create subdirectory in test/files/jvm for your test. Let's name it $TESTDIR.
 * 2. Create $TESTDIR/BytecodeSrc_1.scala that contains Scala source file which you
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

  protected def getMethod(classNode: ClassNode, name: String): MethodNode =
    classNode.methods.asScala.find(_.name == name) getOrElse
      sys.error(s"Didn't find method '$name' in class '${classNode.name}'")

  protected def loadClassNode(name: String): ClassNode = {
    val classBytes: InputStream = (for {
      classRep <- classpath.findClass(name)
      binary <- classRep.binary
    } yield binary.input) getOrElse sys.error(s"failed to load class '$name'; classpath = $classpath")

    val cr = new ClassReader(classBytes)
    val cn = new ClassNode()
    cr.accept(cn, 0)
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
