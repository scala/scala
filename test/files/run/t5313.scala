import scala.tools.partest.IcodeTest

object Test extends IcodeTest {
  override def printIcodeAfterPhase = "dce"

  override def extraSettings: String = super.extraSettings + " -optimize"  

  override def code =
    """class Foo {
      def foo = true
      def bar = {
        var kept1 = new Object
        val result = new java.lang.ref.WeakReference(kept1)
        kept1 = null // we can't eliminate this assigment because result can observe
                   // when the object has no more references. See SI-5313
        var erased2 = null // we can eliminate this store because it's never used
        val erased3 = erased2 // and this
        var erased4 = erased2 // and this
        val erased5 = erased4 // and this
        var kept2: Object = new Object // ultimately can't be eliminated 
        while(foo) {
          val kept3 = kept2
          kept2 = null // this can't, because it clobbers x, which is ultimately used
          erased4 = null // safe to eliminate       
          println(kept3)
        }
        var kept4 = new Object // have to keep, it's used
        try
          println(kept4)
        catch {
          case _ : Throwable => kept4 = null // have to keep, it clobbers kept4 which is used
        }
        result
      }
    }""".stripMargin

  override def show() {
    val storeLocal = "STORE_LOCAL"
    val lines1 = collectIcode("") filter (_ contains storeLocal) map (x => x.drop(x.indexOf(storeLocal)))
    println(lines1 mkString "\n")
  }
}
