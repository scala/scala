object Test {
  class Foo { val z = "foo"; val y : z.type = z }

  val x : ({ val y : z.type } forSome { val z : String }) = new Foo

  val x2 : ({ val y : T } forSome { type T <: String with Singleton }) = new Foo
}
