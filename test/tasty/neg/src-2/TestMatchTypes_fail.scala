package tastytest

object TestMatchTypes {

  def test1: MatchTypes.Elem[List[String]] = "hello"
  def test2: String = new MatchTypes.Foo[List[String], "hello"].foo("hello")

}
