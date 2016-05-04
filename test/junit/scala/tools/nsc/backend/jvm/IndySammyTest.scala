package scala.tools.nsc
package backend.jvm

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._


@RunWith(classOf[JUnit4])
class IndySammyTest extends BytecodeTesting {
  import compiler._

  def funClassName(from: String, to: String) = s"Fun$from$to"
  def classPrologue(from: String, to: String) =
    "class VC(private val i: Int) extends AnyVal\n" +
    s"trait ${funClassName(from, to)} { def apply(a: $from): $to}"

  def lamDef(from: String, to: String, body: String => String) =
    s"""def lam = (x => ${body("x")}): ${funClassName(from, to)}"""

  def appDef(arg: String) = s"""def app = lam($arg)"""

  /* Create a lambda of type "$from => $to" (with body "$body(x)" if "x" is the argument name),
   * and apply it to `arg`.
   *
   * Check:
   *  - the signature of the apply method
   *  - the instructions in the lambda's body (anonfun method)
   *  - the instructions used to create the argument for the application
   *    (and the return corresponding to the lambda's result type)
   */
  def test(from: String, to: String, arg: String, body: String => String = x => x)
          (expectedSig: String, lamBody: List[Instruction], appArgs: List[Instruction], ret: Instruction)
          (allowMessage: StoreReporter#Info => Boolean = _ => false) = {
    val List(funClass, vcClass, vcCompanion) = compileClasses(s"${classPrologue(from, to)}")
    val c = compileClass(s"class C { ${lamDef(from, to, body)}; ${appDef(arg)} }", allowMessage = allowMessage)

    val applySig = getAsmMethod(funClass, "apply").desc
    val anonfun = getMethod(c, "$anonfun$lam$1")
    val lamInsn = getInstructions(c, "lam").dropNonOp
    val applyInvoke = getMethod(c, "app")

    assertEquals(expectedSig, applySig)
    assert(lamInsn.length == 2 && lamInsn.head.isInstanceOf[InvokeDynamic], lamInsn)
    assertSameCode(anonfun, lamBody)
    assertSameCode(applyInvoke, List(
      VarOp(ALOAD, 0),
      Invoke(INVOKEVIRTUAL, "C", "lam", s"()L${funClassName(from, to)};", false)) ++ appArgs ++ List(
      Invoke(INVOKEINTERFACE, funClassName(from, to), "apply", applySig, true), ret)
    )
  }

//  def testSpecial(lam: String, lamTp: String, arg: String)(allowMessage: StoreReporter#Info => Boolean = _ => false) = {
//    val cls = compileClasses("trait Special[@specialized A] { def apply(a: A): A}" )
//    val methodNodes = compileMethods(compiler)(s"def lam : $lamTp = $lam" +";"+ appDef(arg), allowMessage)
//
//    val anonfun = methodNodes.filter(_.name contains "$anonfun$").map(convertMethod)
//    val lamInsn = methodNodes.find(_.name == "lam").map(instructionsFromMethod).get.dropNonOp
//    val applyInvoke = methodNodes.find(_.name == "app").map(convertMethod).get
//
//    assert(lamInsn.length == 2 && lamInsn.head.isInstanceOf[InvokeDynamic], lamInsn)
//    assertSameCode(anonfun, lamBody)
//    assertSameCode(applyInvoke, List(
//      VarOp(ALOAD, 0),
//      Invoke(INVOKEVIRTUAL, "C", "lam", s"()L${funClassName(from, to)};", false)) ++ appArgs ++ List(
//      Invoke(INVOKEINTERFACE, funClassName(from, to), "apply", applySig, true), ret)
//    )
//  }

  // x => x : VC => VC applied to VC(1)
  @Test
  def testVC_VC_VC =
    test("VC", "VC", "new VC(1)")("(I)I",
      List(VarOp(ILOAD, 0), Op(IRETURN)),
      List(Op(ICONST_1)),
      Op(IRETURN))()

  // x => new VC(x) : Int => VC applied to 1
  @Test
  def testInt_VC_1 =
    test("Int", "VC", "1", x => s"new VC($x)")("(I)I",
      List(VarOp(ILOAD, 0), Op(IRETURN)),
      List(Op(ICONST_1)),
      Op(IRETURN))()

  // x => x : VC => Int applied to VC(1)
  @Test
  def testVC_Int_VC =
    test("VC", "Int", "new VC(1)", x => "1")("(I)I",
      List(Op(ICONST_1), Op(IRETURN)),
      List(Op(ICONST_1)),
      Op(IRETURN))()

  // x => new VC(1) : VC => Any applied to VC(1)
  @Test
  def testVC_Any_VC =
    test("VC", "Any", "new VC(1)", x => s"new VC(1)")("(I)Ljava/lang/Object;",
      List(TypeOp(NEW, "VC"), Op(DUP), Op(ICONST_1), Invoke(INVOKESPECIAL, "VC", "<init>", "(I)V", false), Op(ARETURN)),
      List(Op(ICONST_1)),
      Op(ARETURN))()


  // x => x : VC => Unit applied to VC(1)
  @Test
  def testVC_Unit_VC =
    test("VC", "Unit", "new VC(1)")("(I)V",
      List(VarOp(ILOAD, 0), Op(POP), Op(RETURN)),
      List(Op(ICONST_1)),
      Op(RETURN))(allowMessage = _.msg.contains("pure expression"))

  // x => new VC(x.asInstanceOf[Int]) : Any => VC applied to 1
  //
  // Scala:
  //   def lam = (x => new VC(x.asInstanceOf[Int])): FunAny_VC
  //   def app = lam(1)
  // Java:
  //   FunAny_VC  lam() { return x -> BoxesRunTime.unboxToInt((Object)x); }
  //   int    app()    { lam().apply(BoxesRunTime.boxToInteger((int)1));
  @Test
  def testAny_VC_1 =
    test("Any", "VC", "1", x => s"new VC($x.asInstanceOf[Int])")("(Ljava/lang/Object;)I",
      List(VarOp(ALOAD, 0), Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false), Op(IRETURN)),
      List(Op(ICONST_1), Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", false)),
      Op(IRETURN))()

  // TODO
  // x => x : Special[Int] applied to 1
//  @Test
//  def testSpecial_Int_1 =
//    testSpecial("x => x", "Special[Int]", "1")()


  // Tests ThisReferringMethodsTraverser
  @Test
  def testStaticIfNoThisReference: Unit = {
    val methodNodes = compileAsmMethods("def foo = () => () => () => 42")
    methodNodes.forall(m => !m.name.contains("anonfun") || (m.access & ACC_STATIC) == ACC_STATIC)
  }
}
