trait Foo[@specialized(Int) A] {
  final def bar(a:A):A = bar(a)
}
