
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  //def code = ":paste < EOF\n" + (
  def code = 
    """
:paste < EOF
class C { def c = 42 }
EOF
new C().c
:paste <| EOF
  |class D { def d = 42 }
EOF
new D().d
    """
  //)
}
