object Test {
  def foo[A, B, C](l: List[A], f: A => B=>B, g: B=>B=>C): List[C] = l map (g compose f)
}
