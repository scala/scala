object Test extends App {
  import reflect.runtime.universe._ // not using the XML library in compiler tests

  def show(code: String, t: Tree) = println(s"\n$code\n$t")

  val ns1 = "ns1"
  show("<sample xmlns='ns1'/>", q"<sample xmlns='ns1'/>")
  show("<sample xmlns={identity(ns1)}/>", q"<sample xmlns={ns1}/>")
  show("<sample xmlns:foo='ns1'/>", q"<sample xmlns:foo='ns1'/>")
  show("<sample xmlns:foo={identity(ns1)}/>", q"<sample xmlns:foo={ns1}/>")

  // `identity(foo)` used to match the overly permissive match in SymbolXMLBuilder
  // which was intended to more specifically match `_root_.scala.xml.Text(...)`
}
