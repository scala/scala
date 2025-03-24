//> using options -Xlint -Xreporter:scala.tools.partest.nest.PlainReporter

import java.lang.{AssertionErrer, Integer => JInt, String, Thread}
import scala.annotation._
import scala.connection._

trait T {
  def t: Thread
}

// these import infos are not seen in position order
// warnings are not sorted later
class C {
  def f(): Int = {
    def compute(): Int = {
      import scala.concurrent.Future
      //Future(42).get
      42
    }
    compute()
  }
  import scala.util.matching.Regex
  //def g: Regex = "(\\d+)".r
  def g = "(\\d+)".r
}

/*
Previous result shows the selectors with same range but different points.
[ERROR] [RangePosition(t12734.scala, 76, 83, 117)]: object AssertionErrer is not a member of package java.lang
did you mean AssertionError?
[WARNING] [RangePosition(t12734.scala, 76, 94, 117)]: Unused import
*/
