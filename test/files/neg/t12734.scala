//> using options -Xlint -Xreporter:scala.tools.partest.nest.PlainReporter

import java.lang.{AssertionErrer, Integer => JInt, String, Thread}
import scala.annotation._
import scala.connection._

trait T {
  def t: Thread
}

/*
Previous result shows the selectors with same range but different points.
[ERROR] [RangePosition(t12734.scala, 76, 83, 117)]: object AssertionErrer is not a member of package java.lang
did you mean AssertionError?
[WARNING] [RangePosition(t12734.scala, 76, 94, 117)]: Unused import
*/
