



class DelayedInitTest {
  def a = ()
  class B extends DelayedInit {
    a
    def delayedInit(body: => Unit) = ()
  }
}
