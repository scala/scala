object Test extends App {
  import math.clamp

  assert(clamp(Char.MinValue, Char.MaxValue)(Char.MinValue) == Char.MinValue)
  assert(clamp(Char.MinValue, Char.MaxValue)(Char.MaxValue) == Char.MaxValue)
  assert(clamp('s', 'u')('a') == 's')
  assert(clamp('s', 'u')('s') == 's')
  assert(clamp('s', 'u')('t') == 't')
  assert(clamp('s', 'u')('z') == 'u')
  assert(clamp('s', 'u')('u') == 'u')

  assert(clamp(Byte.MinValue, Byte.MaxValue)(Byte.MinValue) == Byte.MinValue)
  assert(clamp(Byte.MinValue, Byte.MaxValue)(Byte.MaxValue) == Byte.MaxValue)
  assert(clamp(0.toByte, 100.toByte)( -5.toByte) ==   0.toByte)
  assert(clamp(0.toByte, 100.toByte)(  0.toByte) ==   0.toByte)
  assert(clamp(0.toByte, 100.toByte)( 50.toByte) ==  50.toByte)
  assert(clamp(0.toByte, 100.toByte)(100.toByte) == 100.toByte)
  assert(clamp(0.toByte, 100.toByte)(105.toByte) == 100.toByte)

  assert(clamp(Int.MinValue, Int.MaxValue)(Int.MinValue) == Int.MinValue)
  assert(clamp(Int.MinValue, Int.MaxValue)(Int.MaxValue) == Int.MaxValue)
  assert(clamp(0, 100)( -5) ==   0)
  assert(clamp(0, 100)(  0) ==   0)
  assert(clamp(0, 100)( 50) ==  50)
  assert(clamp(0, 100)(100) == 100)
  assert(clamp(0, 100)(105) == 100)

  assert(clamp(Long.MinValue, Long.MaxValue)(Long.MinValue) == Long.MinValue)
  assert(clamp(Long.MinValue, Long.MaxValue)(Long.MaxValue) == Long.MaxValue)
  assert(clamp(0L, 100L)( -5L) ==   0L)
  assert(clamp(0L, 100L)(  0L) ==   0L)
  assert(clamp(0L, 100L)( 50L) ==  50L)
  assert(clamp(0L, 100L)(100L) == 100L)
  assert(clamp(0L, 100L)(105L) == 100L)

  assert(java.lang.Float.compare(clamp(Float.MinValue, Float.MaxValue)(Float.MinValue), Float.MinValue) == 0)
  assert(java.lang.Float.compare(clamp(Float.MinValue, Float.MaxValue)(Float.MaxValue), Float.MaxValue) == 0)
  assert(java.lang.Float.compare(clamp(0.0f, 1.0f)(-10.0f), 0.0f) == 0)
  assert(java.lang.Float.compare(clamp(0.0f, 1.0f)(  0.0f), 0.0f) == 0)
  assert(java.lang.Float.compare(clamp(0.0f, 1.0f)(  0.5f), 0.5f) == 0)
  assert(java.lang.Float.compare(clamp(0.0f, 1.0f)(  1.0f), 1.0f) == 0)
  assert(java.lang.Float.compare(clamp(0.0f, 1.0f)( 10.0f), 1.0f) == 0)

  assert(java.lang.Double.compare(clamp(Double.MinValue, Double.MaxValue)(Double.MinValue), Double.MinValue) == 0)
  assert(java.lang.Double.compare(clamp(Double.MinValue, Double.MaxValue)(Double.MaxValue), Double.MaxValue) == 0)
  assert(java.lang.Double.compare(clamp(0.0, 1.0)(-10.0), 0.0) == 0)
  assert(java.lang.Double.compare(clamp(0.0, 1.0)(  0.0), 0.0) == 0)
  assert(java.lang.Double.compare(clamp(0.0, 1.0)(  0.5), 0.5) == 0)
  assert(java.lang.Double.compare(clamp(0.0, 1.0)(  1.0), 1.0) == 0)
  assert(java.lang.Double.compare(clamp(0.0, 1.0)( 10.0), 1.0) == 0)
  
  import scala.math.BigInt
  assert(clamp(BigInt(0), BigInt(100))(BigInt( -5)) == BigInt(  0))
  assert(clamp(BigInt(0), BigInt(100))(BigInt(  0)) == BigInt(  0))
  assert(clamp(BigInt(0), BigInt(100))(BigInt( 50)) == BigInt( 50))
  assert(clamp(BigInt(0), BigInt(100))(BigInt(100)) == BigInt(100))
  assert(clamp(BigInt(0), BigInt(100))(BigInt(105)) == BigInt(100))

  import scala.math.BigDecimal
  assert(clamp(BigDecimal(0), BigDecimal(100))(BigDecimal( -5)) == BigDecimal(  0))
  assert(clamp(BigDecimal(0), BigDecimal(100))(BigDecimal(  0)) == BigDecimal(  0))
  assert(clamp(BigDecimal(0), BigDecimal(100))(BigDecimal( 50)) == BigDecimal( 50))
  assert(clamp(BigDecimal(0), BigDecimal(100))(BigDecimal(100)) == BigDecimal(100))
  assert(clamp(BigDecimal(0), BigDecimal(100))(BigDecimal(105)) == BigDecimal(100))
 }
