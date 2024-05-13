// scalac: -Xasync

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

object Test {
  def main(args: Array[String]): Unit = {
    assertEquals(Some(22), sw1(11))
    assertEquals(Some(3), sw1(3))

    assertEquals(Some(22), sw2(11))
    assertEquals(Some(3), sw2(3))

    assertEquals(Some(22), sw3(11))
    assertEquals(Some(44), sw3(22))
    assertEquals(Some(3), sw3(3))

    assertEquals(Some("22"), swS("11"))
    assertEquals(Some("3"), swS("3"))
  }

  private def sw1(i: Int) = optionally {
    i match {
      case 11 if value(Some(430)) > 42 => 22
      case p => p
    }
  }

  private def sw2(i: Int) = optionally {
    i match {
      case 11 => if (value(Some(430)) > 42) 22 else i
      case p => p
    }
  }

  private def sw3(i: Int) = optionally {
    i match {
      case 11 => if (value(Some(430)) > 42) 22 else i
      case 22 | 33 => 44
      case p => p
    }
  }

  private def swS(s: String) = optionally {
    s match {
      case "11" if value(Some(430)) > 42 => "22"
      case p => p
    }
  }
}
