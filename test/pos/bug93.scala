object Bug {
  def f(def cond: Boolean) = while (cond == false) {};
  // no bug with "false == cond"
}
