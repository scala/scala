package scala

import org.junit.Assert._
import org.junit.Test

final class UnitTest {
  @Test def testVoid(): Unit = assertTrue(().getClass == Unit.void(1).getClass)
}
