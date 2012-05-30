object Test extends App {
  val foo = scala.xml.Attribute(null, "foo", "1", scala.xml.Null)
  val bar = scala.xml.Attribute(null, "bar", "2", foo)
  println(foo)
  println(bar)
  println(scala.xml.TopScope.getURI(foo.pre))
  println(bar remove "foo")
  println(bar remove "bar")
  println(bar remove (null, scala.xml.TopScope, "foo"))
  println(bar remove (null, scala.xml.TopScope, "bar"))

  val ns = scala.xml.NamespaceBinding(null, "uri", scala.xml.TopScope)
  println(bar remove (null, ns, "foo"))
  println(bar remove (null, ns, "bar"))
}
