import t1.Vis

abstract class Foo extends t1.Base {
  trait Nested {
    def crash() {
      inner
    }
  }
}
