import scala.tools.partest.ReplTest

object Test extends ReplTest {
	def code = """:power
:paste
{
  val clazz = rootMirror.getClassByName(newTermName("test.ThrowsDeclaration_2"));
  {
  	val method = clazz.info.member(newTermName("foo"))
  	val throwsAnn = method.annotations.head
  	val atp = throwsAnn.atp
  	println("foo")
  	println("atp.typeParams.isEmpty: " + atp.typeParams.isEmpty)
  	println(throwsAnn)
  }
  println

  {
  	val method = clazz.info.member(newTermName("bar"))
  	val throwsAnn = method.annotations.head
  	val Literal(const) = throwsAnn.args.head
  	val tp = const.typeValue
  	println("bar")
  	println("tp.typeParams.isEmpty: " + tp.typeParams.isEmpty)
  	println(throwsAnn)
  }
}
"""
}
