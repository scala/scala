import scala.tools.partest.instrumented._
import scala.tools.partest.instrumented.Instrumentation._

object Test {
  def main(args: Array[String]): Unit = {
    'warmup
    startProfiling()
    var i = 0;
    while (i < 2) {
      'foo.name
      i += 1
    }
    stopProfiling()
    // Only expect a single call to lookup the interned Symbol at each call site the defines
    // a single literal.
    val Symbol_apply = MethodCallTrace("scala/Symbol$", "apply", "(Ljava/lang/String;)Lscala/Symbol;")
    assert(getStatistics.get(Symbol_apply) == Some(1), getStatistics);
  }
}
