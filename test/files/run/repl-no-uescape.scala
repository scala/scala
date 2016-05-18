import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings

/*
scala> object A
<console>:10: error: invalid escape character
+ "defined object " + "A" + "\u000A"

Under -Dscala.color=true control chars are common
  $eval.this.$print = {
    $line2.$read.$iw.$iw;
    "\033[1m\033[34mres1\033[0m: \033[1m\033[32mInt\033[0m = ".+(scala.runtime.ScalaRunTime.replStringOf($line2.$read.$iw.$iw.res1, 1000))
  };

$ skala -Dscala.color=true -Xno-uescape
Welcome to Scala 2.11.9-20160323-163638-1fcfdd8c8b (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_60).
Type in expressions for evaluation. Or try :help.

scala> 42
<console>:10: error: invalid escape character
 + "\u001B[1m\u001B[34mres0\u001B[0m: \u001B[1m\u001B[32mInt\u001B[0m = " + scala.runtime.ScalaRunTime.replStringOf(res0, 1000)
 */
object Test extends ReplTest {
  override def transformSettings(settings: Settings): Settings = {
    settings.nouescape.value = true
    settings
  }
  def code = """
object A
  """
}
