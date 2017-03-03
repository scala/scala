import scala.tools.partest.ReplTest

// confirm X not in empty package
object Test extends ReplTest {
  def code =
    """
:paste < EOF
object X
EOF
assert(X.getClass.getName.contains("line"))
"""

}
