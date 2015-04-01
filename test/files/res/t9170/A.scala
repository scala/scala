object Y {
  def f[A](a: =>  A) = 1
  def f[A](a: => Either[Exception, A]) = 2
}
