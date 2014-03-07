object Bug {
  trait Sys[S] {
    type Tx
  }

  trait Baz[-Tx]

  trait Foo[S <: Sys[S]] {
    def bar: Bar[S] = Bar.read[S]()
  }

  object Bar {
    object NoBaz extends Baz[Any]

    def read[S <: Sys[S]](baz: Baz[S#Tx] = NoBaz): Bar[S] = ???
  }
  trait Bar[S <: Sys[S]]
}