import scala.reflect.mirror._

object Test extends App {
  class MyQuerycollection{
    def findUserByName( name:String ) = {
      val tree = reify{ "test" == name }.tree
      val toolbox = mkToolBox()
      toolbox.typeCheck(tree) match{
        case Apply(Select(lhs,op),rhs::Nil) =>
          println(rhs.tpe)
      }
    }
  }
  val qc = new MyQuerycollection
  qc.findUserByName("some value")
}
