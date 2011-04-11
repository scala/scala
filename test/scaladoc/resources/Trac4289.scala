class Superclass {
  /**
   * @return 123
   */
  def foo = 123
}

class Subclass extends Superclass {
  /**
   * hello
   */
  override def foo = 456
}
