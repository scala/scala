case class C();

object arrays2 {

  def main(args: Array[String]): Unit = {
    val a: Array[Array[C]] = new Array[Array[C]](2);
    a(0) = new Array[C](2);
    a(0)(0) = new C();
  }
}

// #2422
object arrays4 {
  val args = Array[String]("World") 
  "Hello %1$s".format(args: _*) 
}

// #2461
object arrays3 {
  import scala.collection.JavaConversions._
  def apply[X](xs : X*) : java.util.List[X] = java.util.Arrays.asList(xs: _*)
}

