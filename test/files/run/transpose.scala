object Test {
  def wrap[T >: Null](body: => T) = 
    try body
    catch { case _: IllegalArgumentException => null }
  
  def main(args: Array[String]): Unit = {
    assert(wrap(Nil.transpose) == Nil)
    assert(wrap(List(List(1, 2), List(1)).transpose) == null)
    assert(wrap(List(List(1), List(1, 2)).transpose) == null)
    assert(wrap(List(List(1, 2), List(1, 2)).transpose) == List(List(1, 1), List(2, 2)))
  }
}
