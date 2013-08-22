object Test {
  def op1_:(x: => Any) = ()                 // warn
  def op2_:(x: Any, y: => Any) = ()         // warn
  def op3_:(x: Any, y: => Any)(a: Any) = () // warn

  def op4() = ()                            // no warn
  def op5(x: => Any) = ()                   // no warn
  def op6_:(x: Any)(a: => Any) = ()         // no warn
}
