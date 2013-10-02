object Test {
  def foo(a: A) = a.name

  type TP = X // already failed before this fix
}
