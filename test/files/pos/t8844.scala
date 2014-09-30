object Example {
  type S[A] = String
  def foo(s: S[_]): String = s
}
