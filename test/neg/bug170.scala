trait J {
  def foo(): Unit;
}

trait I with J {
  override def foo(): Unit = ();
}

class D extends J with I {
  abstract override def foo(): Unit = super.foo();
}

object Test with Executable {
  (new D).foo();
}
