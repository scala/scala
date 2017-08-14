package b {
  import a._

  object C {
    def m = {
      val a = new A()
        .setConnectTimeout(1)
        .setFailedAttempts(1)
      0
    }
  }
}

object Test extends App {
  assert(b.C.m == 0)
}
