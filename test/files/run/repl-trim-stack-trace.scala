import scala.tools.partest.{SessionTest, StackCleaner}

// scala/bug#7740 pretty print stack traces
//
object Test extends SessionTest with StackCleaner
