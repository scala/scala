trait Foo[@specialized(Int) A] {
  final def bar(a:A):A = {
    val b = a
    bar(a)
  }
}
