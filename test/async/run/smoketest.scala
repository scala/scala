//> using options -Xasync

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

// Scala.js compatible test suite for -Xasync that doesn't use Scala futures
object Test {
  def main(args: Array[String]): Unit = {
    testBasic()
    testWhile()
    testWhileNested()
    ifExpression()
    testPatternCascade()
  }

  private def testBasic() = {
    assertEquals(Some(3), optionally(value(Some(1)) + value(Some(2))))
  }

  private def testWhile() = {
    assertEquals(Some((0 until 10).sum), optionally {
      var i = 0
      var z = 0
      while (i < 10) {
        z += value(Some(i))
        i += 1
      }
      z
    })
  }

  private def testWhileNested() = {
    assertEquals(Some((0 until 10).sum + 10), optionally {
      var i = 0
      var z = 0
      while (i < 10) {
        z += value(Some(i))
        var j = 0
        while (j < value(Some(1))) {
          z += value(Some(1))
          j += 1
        }
        i += 1
      }
      z
    })
  }

  private def ifExpression() = {
    assertEquals(Some(42), optionally {
      if (if (value(Some(true))) value(Some(true)) else false) {
        value(Some(if (value(Some(true))) 42 else ???))
      } else {
        ???
      }
    })
  }

  private def testPatternCascade() = {
    assertEquals(Some(true), optionally {
      (Some("x"), Option.empty[String]) match {
        case (Some("y"), _) => ???
        case (Some("x"), Some(x)) if value(Some(true)) => ???
        case (Some("x"), None) if value(Some(false)) => ???
        case (Some("x"), None) if value(Some(true)) => true
        case _ => ???
      }
    })
  }
}