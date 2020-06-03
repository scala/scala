
object Test extends scala.tools.partest.ReplTest {
  def code = """:paste <| EOF
    |// comment 1
    |val x = 3
    |// comment 2
    |val y = 4
EOF
(x, y)
  """
}
