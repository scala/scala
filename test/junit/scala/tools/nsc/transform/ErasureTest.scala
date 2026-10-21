package scala.tools.nsc.transform

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting.{assertNoInvoke, findClass, getAsmMethods, getMethod}

class ErasureTest extends BytecodeTesting {
  object symbolTable extends SymbolTableForUnitTesting

  @Test def testGenericArray(): Unit = {
    import symbolTable._
    import definitions._
    val T = NoSymbol.newTypeParameter(TypeName("T")).setInfo(TypeBounds.empty)
    val U = NoSymbol.newTypeParameter(TypeName("U")).setInfo(TypeBounds.empty)
    val arrayTWithArrayString: Type = refinedType(appliedType(ArrayClass, T.tpeHK) :: appliedType(ArrayClass, StringTpe) :: Nil, NoSymbol)
    val arrayTWithArrayU: Type = refinedType(appliedType(ArrayClass, T.tpeHK) :: appliedType(ArrayClass, U.tpeHK) :: Nil, NoSymbol)

    assertEquals(1, erasure.unboundedGenericArrayLevel(appliedType(ArrayClass, T.tpeHK)))
    assertEquals(0, erasure.unboundedGenericArrayLevel(arrayTWithArrayString))
    assertEquals(1, erasure.unboundedGenericArrayLevel(arrayTWithArrayU))
  }

  import compiler._

  @Test
  def noScalaRuntimeForRefinedTypeWithGenericAndConcreteArray(): Unit = {
    val code =
      """class C {
        |  def t[T](xs: Array[T]): Unit = xs match {
        |    case x: Array[AnyRef] => x(0)
        |    case _ =>
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertNoInvoke(getMethod(c, "t"))
  }

  /** The casts and calls in the body of trait method (or lambda body) `name` of class `cls`. */
  private def castsAndCalls(classes: List[ClassNode], cls: String, name: String): List[Instruction] = {
    val c = findClass(classes, cls)
    val ms = getAsmMethods(c, name).filterNot(m => BytecodeUtils.isStaticMethod(m) && getAsmMethods(c, name).sizeIs > 1)
    assertEquals(s"$cls.$name", 1, ms.size)
    instructionsFromMethod(ms.head).filter {
      case TypeOp(CHECKCAST, _) | _: Invoke => true
      case _ => false
    }
  }

  @Test
  def selfTypeClassCast(): Unit = {
    // a cake: traits self-typed with the class that mixes them in, as SymbolTable
    val code =
      """trait Defs { self: U => def defs: String = "d" }
        |trait Types { self: U =>
        |  def viaThis: String = defs
        |  def viaSelf: String = self.defs
        |  def viaLambda: () => String = () => defs
        |  def own: String = viaThis
        |  def classMember: String = fromClass
        |  class Inner { def viaOuter: String = defs }
        |}
        |abstract class U extends Defs with Types { def fromClass: String = "c" }
        |""".stripMargin
    val cs = compileClasses(code)
    val viaClass = List(TypeOp(CHECKCAST, "U"), InvokeVirtual("U", "defs", "()Ljava/lang/String;"))
    // `this` erases to the trait, so a member of another part of the self type needs a cast: to the self type's
    // class, not to the member's owner (`checkcast Defs; invokeinterface Defs.defs`)
    assertEquals(viaClass, castsAndCalls(cs, "Types", "viaThis"))
    assertEquals(viaClass, castsAndCalls(cs, "Types", "viaSelf"))
    assertEquals(viaClass, castsAndCalls(cs, "Types", "$anonfun$viaLambda$1"))
    // as through an inner class's `$outer`, which is typed with the erased self type
    assertEquals(List(InvokeVirtual("Types$Inner", "Types$Inner$$$outer", "()LU;"), InvokeVirtual("U", "defs", "()Ljava/lang/String;")),
      castsAndCalls(cs, "Types$Inner", "viaOuter"))
    // unchanged: a member of the trait itself needs no cast; one of the class was already cast to the class
    assertEquals(List(InvokeInterface("Types", "viaThis", "()Ljava/lang/String;")), castsAndCalls(cs, "Types", "own"))
    assertEquals(List(TypeOp(CHECKCAST, "U"), InvokeVirtual("U", "fromClass", "()Ljava/lang/String;")), castsAndCalls(cs, "Types", "classMember"))
  }

  @Test
  def selfTypeCastToOwnerOtherwise(): Unit = {
    val code =
      """trait D { def d: String = "d" }
        |trait X { def x: String = "x" }
        |abstract class V
        |trait SelfTrait { self: D => def m: String = d }
        |trait SelfRefined { self: V with X => def m: String = x }
        |abstract class Base { def b: String = "b" }
        |trait SelfSub { self: Sub => def m: String = b }
        |abstract class Sub extends Base with SelfSub
        |class P { private def a(): String = "P.a" }
        |trait A { def a(): String }
        |trait SelfPrivate { self: UP => def m: String = a() }
        |abstract class UP extends P with A with SelfPrivate
        |trait SelfJava { self: UJ => def m: String = d }
        |abstract class UJ extends java.lang.Thread with D with SelfJava
        |""".stripMargin
    val cs = compileClasses(code)
    // a self type that erases to a trait: cast to the member's owner, as before
    assertEquals(List(TypeOp(CHECKCAST, "D"), InvokeInterface("D", "d", "()Ljava/lang/String;")), castsAndCalls(cs, "SelfTrait", "m"))
    // `V with X` erases to the class `V`, which does not derive from `X`: cast to `X`
    assertEquals(List(TypeOp(CHECKCAST, "X"), InvokeInterface("X", "x", "()Ljava/lang/String;")), castsAndCalls(cs, "SelfRefined", "m"))
    // a member of a class is already called with `invokevirtual`; it is not retargeted to the self type's class
    assertEquals(List(TypeOp(CHECKCAST, "Base"), InvokeVirtual("Base", "b", "()Ljava/lang/String;")), castsAndCalls(cs, "SelfSub", "m"))
    // `invokevirtual UP.a` would resolve to the private `P.a`
    assertEquals(List(TypeOp(CHECKCAST, "A"), InvokeInterface("A", "a", "()Ljava/lang/String;")), castsAndCalls(cs, "SelfPrivate", "m"))
    // a Java superclass may declare private or static methods that we cannot see
    assertEquals(List(TypeOp(CHECKCAST, "D"), InvokeInterface("D", "d", "()Ljava/lang/String;")), castsAndCalls(cs, "SelfJava", "m"))
  }

  @Test
  def selfTypeCastOverload(): Unit = {
    val code =
      """class P { private def a(x: Int): String = "P.a" }
        |trait A { def a(): String }
        |trait SelfOverload { self: U => def m: String = a() }
        |abstract class U extends P with A with SelfOverload
        |""".stripMargin
    val cs = compileClasses(code)
    // the private `P.a` has a different descriptor, so `invokevirtual U.a` resolves to `A.a`
    assertEquals(List(TypeOp(CHECKCAST, "U"), InvokeVirtual("U", "a", "()Ljava/lang/String;")), castsAndCalls(cs, "SelfOverload", "m"))
  }
}
