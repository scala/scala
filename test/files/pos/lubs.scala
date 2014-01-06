object Test {
  List(new { def f = 1; def g = 1}, new { def f = 2}).map(_.f)
}
