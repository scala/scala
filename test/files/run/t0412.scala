object Test extends Application {
  println(classOf[java.util.ArrayList[_]])
  println(classOf[java.util.ArrayList[T] forSome { type T }])
}
