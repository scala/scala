
import scala.tools.partest.ReplTest

object Test extends ReplTest {

  def code = """
import Predef.{toString => _}

class C {
  def f: Int = 42.ensuring(_ > 27)
}
  """
}
/* was:
import Predef.toString=>_

import Predef.toString=>_
^
<synthetic>:1: error: ';' expected but '=>' found.

(To diagnose errors in synthetic code, try adding `// show` to the end of your input.)
 */
