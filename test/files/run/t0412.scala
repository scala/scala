object Test extends App {
  println(classOf[java.util.ArrayList[_]])
  println(classOf[java.util.ArrayList[T] forSome { type T }])
}
