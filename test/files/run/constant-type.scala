import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
:power
val s = transformedType(StringClass.toType).asInstanceOf[Type]
{ println(afterPhase(currentRun.erasurePhase)(ConstantType(Constant(s)))); }
{ ConstantType(Constant(s)); println(afterPhase(currentRun.erasurePhase)(ConstantType(Constant(s)))); }
  """
}
