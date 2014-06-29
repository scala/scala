import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
  class C {
    
    /**
     * New in release 1.2.3.4, it works. Next sentence.
     * Next Line.
     */
    def method1 = 0

    /** Sentence no period */
    def method2 = 0

    /** Sentence period at end.*/
    def method3 = 0
  }
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    val ms = List("method1", "method2", "method3")
    for (m <- ms) {
      val method = root._class("C")._method(m)
      println(method.comment.get.body.summary)  
    }
  }
}
