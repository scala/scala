object Test {
  def foo() {
    try {
      for (i <- 1 until 5) return
    } catch {
      case _: NullPointerException | _: RuntimeException =>
        // was: "catch block may intercept non-local return from method check"
    }
  }
}
