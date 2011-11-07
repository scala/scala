object Test {
  import scala.collection.mutable._
  
  val s = new Stack[Int]
  s ++= List(1,2,3)
  s map (_ + 1)
  s foreach (_ => ())
  
  def main(args: Array[String]): Unit = {
    
  }
}
