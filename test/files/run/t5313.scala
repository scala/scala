import scala.tools.partest.IcodeTest

object Test extends IcodeTest {
  override def printIcodeAfterPhase = "dce"

  override def extraSettings: String = super.extraSettings + " -optimize"  

  override def code =
    """class Foo {
      def bar = {
        var obj = new Object
        val result = new java.lang.ref.WeakReference(obj)
        obj = null // we can't eliminate this assigment because result can observe
                   // when the object has no more references. See SI-5313
        result
      }
    }""".stripMargin

  override def show() {
    val lines1 = collectIcode("")
    println(lines1 mkString "\n")
  }
}
