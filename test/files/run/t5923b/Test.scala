object Test extends App {
  import scala.collection.generic.CanBuildFrom
  val cbf = implicitly[CanBuildFrom[Nothing, Nothing, Array[Nothing]]]
  println(cbf().result.getClass)
  println(new Array[Nothing](0).getClass)
  println(Array[Nothing]().getClass)
}