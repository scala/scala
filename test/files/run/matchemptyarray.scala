object Test extends Application{
  Array[String]() match {
    case x@Array() => println(x.deep.toString());
  }
}
