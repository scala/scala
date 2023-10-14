class C {
  def test(c: Child): Unit = {
    c.f() // always ok
    c.f // should work too
    c.f(1)
    c.f.toString
  }

  // The issue was first detected on NIO buffers,
  // (on Java 11+), so these should pass now.
  def buffer(c: java.nio.ByteBuffer): Unit = {
    c.position
    c.position(10).position.toString
  }
}
