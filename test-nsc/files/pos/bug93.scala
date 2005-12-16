object Bug {
  def f(cond: => Boolean) = while (cond == false) {};
  // no bug with "false == cond"
}
