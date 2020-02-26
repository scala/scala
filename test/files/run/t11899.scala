import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
:paste < DONE
type T
DONE
42
  """.trim
}
/* was:
scala> :paste < DONE
// Entering paste mode (DONE to finish)

type T
DONE

// Exiting paste mode, now interpreting.

The pasted code is incomplete!

No error found in incomplete source.

 * previously:
The pasted code is incomplete! ...but compilation found no error? Good luck with that.
 */
