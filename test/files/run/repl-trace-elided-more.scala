import scala.tools.partest.{SessionTest, StackCleaner}

// scala/bug#11945 print stack trace elided count correctly
//
object Test extends SessionTest with StackCleaner
