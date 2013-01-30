object Test extends App {
  val foo = <x:foo xmlns:x="http://foo.com/"><x:bar xmlns:x="http://bar.com/"><x:baz/></x:bar></x:foo>
  assert(foo.child.head.scope.toString == """ xmlns:x="http://bar.com/"""")

  val fooDefault = <foo xmlns="http://foo.com/"><bar xmlns="http://bar.com/"><baz/></bar></foo>
  assert(fooDefault.child.head.scope.toString == """ xmlns="http://bar.com/"""")

  val foo2 = scala.xml.XML.loadString("""<x:foo xmlns:x="http://foo.com/"><x:bar xmlns:x="http://bar.com/"><x:baz/></x:bar></x:foo>""")
  assert(foo2.child.head.scope.toString == """ xmlns:x="http://bar.com/"""")

  val foo2Default = scala.xml.XML.loadString("""<foo xmlns="http://foo.com/"><bar xmlns="http://bar.com/"><baz/></bar></foo>""")
  assert(foo2Default.child.head.scope.toString == """ xmlns="http://bar.com/"""")
}
