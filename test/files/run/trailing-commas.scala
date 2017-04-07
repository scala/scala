object Test extends scala.tools.partest.ReplTest {
  def code = """
// test varargs in patterns
val List(x, y, _*,
) = 42 :: 17 :: Nil
"""
}
