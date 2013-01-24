import scala.tools.partest.ReplTest

object Test extends ReplTest {
	def code = """:power
:paste
{
  val clazz = rootMirror.getClassByName(newTermName("t7009.ThrowsDeclaration_1"))
  val method = clazz.info.member(newTermName("foo"))
  val throwsAnn = method.annotations.head
  val atp = throwsAnn.atp
  println("atp.typeParams.isEmpty: " + atp.typeParams.isEmpty)
  println(throwsAnn)
}
"""
}
