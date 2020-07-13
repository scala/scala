package scaladoc.resources

trait T {

  /** $a */
  def foo: Int

}

object O extends T {

  val foo = 42

}
