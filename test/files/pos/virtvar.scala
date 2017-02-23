trait Test extends EmbeddedControls {
  trait Var[T]
  def __assign[T](v:Var[T],x:T):Unit
  var mydefs: List[Any] = Nil
  def test {
    mydefs = Nil
  }
}
