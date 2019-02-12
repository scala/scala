
object Test {
  trait Foo[T]

  trait Bar {
    def -:(core: AnyRef): Foo[core.type] = ???
  }

  val tempval: Bar = ???
  val cor: AnyRef = ???

  val ok : Foo[cor.type] = tempval.-:(cor)
  val oops : Foo[cor.type] = cor -: tempval
}
