object SyntheticNonSynthetic2 {
  def foo[A >: Exception] (a : A) {}
}
