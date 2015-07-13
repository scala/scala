
import scala.tools.partest.SessionTest

// Handy hamburger helper for repl resources
object Test extends SessionTest {
  def session =
"""
scala> $intp.isettings.unwrapStrings = false
$intp.isettings.unwrapStrings: Boolean = false

scala> class Bippy
defined class Bippy

scala> $intp.classLoader getResource "Bippy.class"
res0: java.net.URL = memory:(memory)/$line4/$read$$iw$$iw$Bippy.class

scala> ($intp.classLoader getResources "Bippy.class").nextElement
res1: java.net.URL = memory:(memory)/$line4/$read$$iw$$iw$Bippy.class

scala> ($intp.classLoader classBytes "Bippy").nonEmpty
res2: Boolean = true

scala> ($intp.classLoader classAsStream "Bippy") != null
res3: Boolean = true

scala> $intp.classLoader getResource "Bippy"
res4: java.net.URL = null

scala> :quit"""
}

