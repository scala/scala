object Test extends App {
  class C

  case class FooSeq(x: Int, y: String, z: C*)

  (FooSeq(1, "a", new C()): @unchecked) match {
    case FooSeq(1, "a", x@_* ) =>
      //println(x.toList)
      x.asInstanceOf[x.type]
      assert(x.isInstanceOf[x.type])
  }
}
