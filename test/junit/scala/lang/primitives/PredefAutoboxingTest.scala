package scala.lang.primitives

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class PredefAutoboxingTest {
  @Test def unboxNullByte() =
    assertEquals(Predef.Byte2byte(null), 0.toByte)

  @Test def unboxNullShort() =
    assertEquals(Predef.Short2short(null), 0.toShort)

  @Test def unboxNullCharacter() =
    assertEquals(Predef.Character2char(null), 0.toChar)

  @Test def unboxNullInteger() =
    assertEquals(Predef.Integer2int(null), 0)

  @Test def unboxNullLong() =
    assertEquals(Predef.Long2long(null), 0L)

  @Test def unboxNullFloat() =
    assertEquals(Predef.Float2float(null), 0F, 0F)

  @Test def unboxNullDouble() =
    assertEquals(Predef.Double2double(null), 0D, 0D)

  @Test def unboxNullBoolean() =
    assertEquals(Predef.Boolean2boolean(null), false)
}
