package scala.collection

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

case class A(x: Int, y: Int)

@RunWith(classOf[JUnit4])
class StringOpsTest {
  // Test for scala/bug#10951
  @Test def mkstring(): Unit = {
    assert("".mkString("") == "")
    assert("".mkString(",") == "")
    assert("a".mkString(",") == "a")
    assert("ab".mkString(",") == "a,b")
    println(A(1,2))

  }
}
