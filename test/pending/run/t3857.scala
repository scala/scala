import scala.tools.partest._

class ScalaGeneric { var s: java.util.Set[String] = _ }
trait ScalaGeneric2Trait { var s: java.util.Set[String] = _ }
class ScalaGeneric2 extends ScalaGeneric2Trait { }

object Test extends SigTest {
  def main(args: Array[String]): Unit = {
    show[ScalaGeneric]()
    show[ScalaGeneric2Trait]()
    show[ScalaGeneric2]()    
  }
}
