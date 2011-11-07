// S.scala
class S extends J[AnyRef]

object Test {    
  def main(args:Array[String]) {
    J.f(classOf[S])
  }
}
