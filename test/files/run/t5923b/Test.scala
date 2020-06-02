object Test extends App {
  import scala.collection.Factory
  val cbf = implicitly[Factory[Nothing, Array[Nothing]]]
  println(cbf.newBuilder.result().getClass)
  println(new Array[Nothing](0).getClass)
  println(Array[Nothing]().getClass)
}
