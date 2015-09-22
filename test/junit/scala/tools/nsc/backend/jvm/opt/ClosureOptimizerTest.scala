package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.collection.generic.Clearable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.nsc.io._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._

import BackendReporting._

import scala.collection.convert.decorateAsScala._
import scala.tools.testing.ClearAfterClass

object ClosureOptimizerTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler(extraArgs = "-Yopt:l:classpath -Yopt-warnings:_")
  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class ClosureOptimizerTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = ClosureOptimizerTest

  val compiler = ClosureOptimizerTest.compiler

  @Test
  def nothingTypedClosureBody(): Unit = {
    val code =
      """abstract class C {
        |  def isEmpty: Boolean
        |  @inline final def getOrElse[T >: C](f: => T) = if (isEmpty) f else this
        |  def t = getOrElse(throw new Error(""))
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)
    val t = c.methods.asScala.toList.find(_.name == "t").get
    val List(bodyCall) = findInstr(t, "INVOKESTATIC C.C$$$anonfun$1 ()Lscala/runtime/Nothing$")
    assert(bodyCall.getNext.getOpcode == ATHROW)
  }

  @Test
  def nullTypedClosureBody(): Unit = {
    val code =
      """abstract class C {
        |  def isEmpty: Boolean
        |  @inline final def getOrElse[T >: C](f: => T) = if (isEmpty) f else this
        |  def t = getOrElse(null)
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)
    val t = c.methods.asScala.toList.find(_.name == "t").get
    val List(bodyCall) = findInstr(t, "INVOKESTATIC C.C$$$anonfun$1 ()Lscala/runtime/Null$")
    assert(bodyCall.getNext.getOpcode == POP)
    assert(bodyCall.getNext.getNext.getOpcode == ACONST_NULL)
  }
}
