object t6161 {
  trait N {
    type Name
  }

  trait N1 extends N {
    class Name {
      type ThisNameType <: Name
      def encode: ThisNameType = ???
    }
  }

  trait S {
    self: N => // change to N1 and it compiles
    type NameType <: Name
  }

  object g extends S with N1

  val n1: g.NameType = ???
  val n2: g.Name = n1.encode
}
