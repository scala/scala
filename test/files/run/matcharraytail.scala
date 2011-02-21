object Test extends App{
  Array("foo", "bar", "baz") match {
    case x@Array("foo", bar @_*) => println(x.deep.toString); println(bar.toString);
    case Array(x, y, z) => println("shouldn't have fallen through");
    case _ => println("default case?!");
  }
}
