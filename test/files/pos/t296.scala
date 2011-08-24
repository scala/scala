object Bug {
  def foo (l: => String) : String = 12 match { case _ => l}
}
