object test {
  abstract class Bar {
    type T
    def bar: Unit
  }
  new Bar {
    type T = Int
    def bar = ()
  }.bar
}

