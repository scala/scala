class Something

trait X {
  type A1
}

class Foo extends X {
  type A1 = Something
  type A2 <: Something
}

class Bar extends Foo {
  override type A1 = Something with Serializable
  override type A2 = Any
}
