/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest

import scala.jdk.CollectionConverters._
import scala.tools.asm.{ClassReader, ClassWriter}
import scala.tools.asm.tree._
import java.io.{InputStream, File => JFile}
import scala.tools.testkit.ASMConverters
import AsmNode._
import scala.tools.nsc.CloseableRegistry

/**
 * Provides utilities for inspecting bytecode using ASM library.
 *
 * HOW TO USE
 * 1. Create subdirectory in test/files/jvm for your test. Let's name it \$TESTDIR.
 * 2. Create \$TESTDIR/BytecodeSrc_1.scala that contains Scala source file that you
 *    want to inspect the bytecode for. The '_1' suffix signals to partest that it
 *    should compile this file first.
 * 3. Create \$TESTDIR/Test.scala:
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
  import ASMConverters._

  /** produce the output to be compared against a checkfile */
  protected def show(): Unit

  def main(args: Array[String]): Unit = show()

  // asserts
  def sameBytecode(methA: MethodNode, methB: MethodNode) = {
    val isa = instructionsFromMethod(methA)
    val isb = instructionsFromMethod(methB)
    if (isa == isb) println("bytecode identical")
    else diffInstructions(isa, isb)
  }

  // Do these classes have all the same methods, with the same names, access,
  // descriptors and generic signatures? Method bodies are not considered, and
  // the names of the classes containing the methods are substituted so they do
  // not appear as differences.
  def sameMethodAndFieldSignatures(clazzA: ClassNode, clazzB: ClassNode) =
    sameCharacteristics(clazzA, clazzB)(_.characteristics)

  // Same as sameMethodAndFieldSignatures, but ignoring generic signatures.
  // This allows for methods which receive the same descriptor but differing
  // generic signatures. In particular, this happens with value classes,
  // which get a generic signature where a method written in terms of the
  // underlying values does not.
  def sameMethodAndFieldDescriptors(clazzA: ClassNode, clazzB: ClassNode) =
    sameCharacteristics(clazzA, clazzB)(_.erasedCharacteristics)

  private def sameCharacteristics(clazzA: ClassNode, clazzB: ClassNode)(f: AsmNode[_] => String): Boolean = {
    val ms1 = clazzA.fieldsAndMethods.toIndexedSeq
    val ms2 = clazzB.fieldsAndMethods.toIndexedSeq
    val name1 = clazzA.name
    val name2 = clazzB.name

    if (ms1.length != ms2.length) {
      println(s"Different member counts in $name1 and $name2")
      false
    }
    else ms1.lazyZip(ms2).forall { (m1, m2) =>
      val c1 = f(m1)
      val c2 = f(m2).replace(name2, name1)
      if (c1 == c2)
        println(s"[ok] $m1")
      else
        println(s"[fail]\n  in $name1: $c1\n  in $name2: $c2")

      c1 == c2
    }
  }

  /**
   * Compare the bytecodes of two methods.
   *
   * For the `similar` function, you probably want to pass [[scala.tools.testkit.ASMConverters.equivalentBytecode]].
   */
  def similarBytecode(methA: MethodNode, methB: MethodNode, similar: (List[Instruction], List[Instruction]) => Boolean) = {
    val isa = instructionsFromMethod(methA)
    val isb = instructionsFromMethod(methB)
    if (isa == isb) println("bytecode identical")
    else if (similar(isa, isb)) println("bytecode similar")
    else diffInstructions(isa, isb)
  }

  def diffInstructions(isa: List[Instruction], isb: List[Instruction]) = {
    val len = Math.max(isa.length, isb.length)
    if (len > 0 ) {
      val width = isa.map(_.toString.length).max
      val lineWidth = len.toString.length
      (1 to len) foreach { line =>
        val isaPadded = isa.map(_.toString) orElse LazyList.continually("")
        val isbPadded = isb.map(_.toString) orElse LazyList.continually("")
        val a = isaPadded(line-1)
        val b = isbPadded(line-1)

        println(s"""$line${" " * (lineWidth-line.toString.length)} ${if (a==b) "==" else "<>"} $a${" " * (width-a.length)} | $b""")
      }
    }
  }

// loading
  protected def getField(classNode: ClassNode, name: String): FieldNode =
    classNode.fields.asScala.find(_.name == name) getOrElse
      sys.error(s"Didn't find field '$name' in class '${classNode.name}'")

  protected def getMethod(classNode: ClassNode, name: String): MethodNode =
    classNode.methods.asScala.find(_.name == name) getOrElse
      sys.error(s"Didn't find method '$name' in class '${classNode.name}'")

  protected def loadClassNode(name: String, skipDebugInfo: Boolean = true): ClassNode = {
    val classBytes: InputStream = classpath.findClassFile(name).map(_.input)
      .getOrElse(sys.error(s"failed to load class '$name'; classpath = $classpath"))

    val cr = new ClassReader(classBytes)
    val cn = new ClassNode()
    cr.accept(cn, if (skipDebugInfo) ClassReader.SKIP_DEBUG else 0)
    cn
  }

  protected lazy val classpath: scala.tools.nsc.util.ClassPath = {
    import scala.tools.nsc.classpath.AggregateClassPath
    import scala.tools.nsc.classpath.ClassPathFactory
    import scala.tools.util.PathResolver.Defaults
    import scala.tools.nsc.Settings
    // logic inspired by scala.tools.util.PathResolver implementation
    // `Settings` is used to check YdisableFlatCpCaching in ZipArchiveFlatClassPath
    val factory = new ClassPathFactory(new Settings(), new CloseableRegistry)
    val containers = factory.classesInExpandedPath(sys.props("partest.output") + java.io.File.pathSeparator + Defaults.javaUserClassPath)
    new AggregateClassPath(containers)
  }
}

object BytecodeTest {
  /** Parse `file` as a class file, transforms the ASM representation with `f`,
   *  and overwrites the original file.
   */
  def modifyClassFile(file: JFile)(f: ClassNode => ClassNode): Unit = {
    val rfile = new reflect.io.File(file)
    def readClass: ClassNode = {
      val cr = new ClassReader(rfile.toByteArray())
      val cn = new ClassNode()
      cr.accept(cn, 0)
      cn
    }

    def writeClass(cn: ClassNode): Unit = {
      val writer = new ClassWriter(0)
      cn.accept(writer)
      val os = rfile.bufferedOutput()
      try {
        os.write(writer.toByteArray)
      } finally {
        os.close()
      }
    }

    writeClass(f(readClass))
  }
}
