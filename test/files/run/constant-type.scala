import scala.tools.partest.ReplTest

// see the commit message to understand what this stuff is about
// just a quick note:
// transformedType returns an erased version of the type
// as explained in the commit message, Type.erasure won't do for this test
// because it does some postprocessing to the result of transformedType
object Test extends ReplTest {
  def code = """
:power
val s = transformedType(StringClass.toType).asInstanceOf[Type]
{ println(afterPhase(currentRun.erasurePhase)(ConstantType(Constant(s)))) }
{ afterPhase(currentRun.erasurePhase)(println(ConstantType(Constant(s)))) }
{ ConstantType(Constant(s)); println(afterPhase(currentRun.erasurePhase)(ConstantType(Constant(s)))); }
{ ConstantType(Constant(s)); afterPhase(currentRun.erasurePhase)(println(ConstantType(Constant(s)))); }
  """
}
