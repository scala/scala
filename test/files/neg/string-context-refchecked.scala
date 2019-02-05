class C {
  final def foo = 1
  s"foo${class D extends C { def foo = 2 }; new D}bar"
}
