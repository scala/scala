object Test extends Application {
  println(Array(Some(1), None, Some(2)).flatten.toList)
}