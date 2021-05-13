package scala.math

import org.scalacheck.{Properties, Gen, Arbitrary}
import Arbitrary.arbitrary
import org.scalacheck.Prop._
import java.math.BigInteger
import java.lang.Character
import scala.util.Random

object BigIntProperties extends Properties("BigInt") {

  val positiveNumberString: Gen[String] = for {
    firstDigit <- Gen.choose('1', '9')
    nDigits <- Gen.choose(0, 30)
    digits <- Gen.listOfN(nDigits, Gen.choose('0', '9'))
  } yield firstDigit +: digits.mkString

  val negativeNumberString: Gen[String] = positiveNumberString.map("-" + _)

  val nonZeroNumberString: Gen[String] = Gen.oneOf(positiveNumberString, negativeNumberString)

  val numberString: Gen[String] = Gen.frequency(1 -> "0", 10 -> positiveNumberString, 10 -> negativeNumberString)

  val bigInteger: Gen[BigInteger] = numberString.map(new BigInteger(_))

  val positiveBigInteger: Gen[BigInteger] = positiveNumberString.map(new BigInteger(_))

  val nonZeroBigInteger: Gen[BigInteger] = nonZeroNumberString.map(new BigInteger(_))

  val bigInt: Gen[BigInt] = numberString.map(BigInt(_))

  val nonZeroSignum: Gen[Int] = Gen.oneOf(-1, 1)

  val byteArray: Gen[Array[Byte]] = Gen.containerOf[Array, Byte](arbitrary[Byte])

  val radix: Gen[Int] = Gen.choose(Character.MIN_RADIX, Character.MAX_RADIX)

  val stringAndRadix: Gen[(String, Int)] = for {
    rdx <- radix
    firstDigit <- Gen.choose(1, rdx - 1).map(Character.forDigit(_, rdx))
    nDigits <- Gen.choose(0, 30)
    digits <- Gen.listOfN(nDigits, Gen.choose(0, rdx - 1).map(i => Character.forDigit(i, rdx)))
  } yield (firstDigit +: digits.mkString, rdx)

  val shift: Gen[Int] = for {
    sign <- Gen.oneOf(-1, 1)
    size <- Gen.size
  } yield sign * size

  val bitIndex: Gen[Int] = Gen.size

  val smallExponent: Gen[Int] = Gen.choose(0, 10)
  val exponent: Gen[Int] = Gen.choose(0, Int.MaxValue)

  val relativelyPrimePositiveBigIntegers: Gen[(BigInteger, BigInteger)] = for {
    x <- positiveBigInteger
    y <- positiveBigInteger
    g = x.gcd(y)
  } yield (x.divide(g), y.divide(g))

  property("longValue") = forAll { (l: Long) => BigInt(l).longValue ?= l }
  property("toLong") = forAll { (l: Long) => BigInt(l).toLong ?= l }

  property("new BigInt(bigInteger = BigInteger.ZERO)") = (new BigInt(bigInteger = BigInteger.ZERO)) == 0
  property("BigInt.apply(i: Int)") = forAll { (i: Int) => BigInt(i) ?= BigInt(BigInteger.valueOf(i)) }
  property("BigInt.apply(l: Long)") = forAll { (l: Long) => BigInt(l) ?= BigInt(BigInteger.valueOf(l)) }
  property("BigInt.apply(x: Array[Byte])") = forAll(bigInteger) { bi => BigInt(bi) ?= BigInt(bi.toByteArray) }
  property("BigInt.apply(0, Array.empty[Byte])") = BigInt(0, Array.empty[Byte]) ?= BigInt(0)
  property("BigInt.apply(signum: Int, magnitude: Array[Byte])") = forAll(nonZeroSignum, byteArray) { (s, ba) => BigInt(s, ba) ?= BigInt(new BigInteger(s, ba)) }
  property("BigInt.apply(x: String)") = forAll(numberString) { s => BigInt(s) ?= BigInt(new BigInteger(s)) }
  property("BigInt.apply(x: String, radix: Int)") = forAll(stringAndRadix) { case (x, radix) => BigInt(x, radix) ?= BigInt(new BigInteger(x, radix)) }
  property("BigInt.probablePrime") = forAll(Gen.choose(2, 100), arbitrary[Long]) { (bl, seed) => BigInt.probablePrime(bl, new Random(seed)).isProbablePrime(100) }
  property("hashCode is unified across primitives (Int)") = forAll { (i: Int) => i.## ?= BigInt(i).## }
  property("hashCode is unified across primitives (Long)") = forAll { (l: Long) => l.## ?= BigInt(l).## }
  property("BigInt equals Double") = forAll { (i: Int) => BigInt(i) == i.toDouble }
  property("BigInt equals Float") = forAll { (s: Short) => BigInt(s) == s.toFloat }
  property("BigInt equals Long") = forAll { (l: Long) => BigInt(l) == l }
  property("BigInt equals BigDecimal") = forAll(numberString) { ns => BigInt(ns) == BigDecimal(ns) }
  property("Double equals BigInt") = forAll { (i: Int) => i.toDouble == BigInt(i) }
  property("Float equals BigInt") = forAll { (s: Short) => s.toFloat == BigInt(s) }
  property("Long equals BigInt") = forAll { (l: Long) => l == BigInt(l) }
  property("BigDecimal equals BigInt") = forAll(numberString) { ns => BigDecimal(ns) == BigInt(ns) }
  property("isValidByte") = forAll { (b: Byte) => BigInt(b).isValidByte }
  property("isValidByte") = !BigInt(Byte.MinValue.toInt - 1).isValidByte
  property("isValidByte") = !BigInt(Byte.MaxValue.toInt + 1).isValidByte
  property("isValidShort") = forAll { (s: Short) => BigInt(s).isValidShort }
  property("isValidShort") = !BigInt(Short.MinValue.toInt - 1).isValidShort
  property("isValidShort") = !BigInt(Short.MaxValue.toInt + 1).isValidShort
  property("isValidChar") = forAll { (c: Char) => BigInt(c).isValidChar }
  property("isValidChar") = !BigInt(Char.MinValue.toInt - 1).isValidChar
  property("isValidChar") = !BigInt(Char.MaxValue.toInt + 1).isValidChar
  property("isValidInt") = forAll { (i: Int) => BigInt(i).isValidInt }
  property("isValidInt") = !BigInt(Int.MinValue.toLong - 1).isValidInt
  property("isValidInt") = !BigInt(Int.MaxValue.toLong + 1).isValidInt
  property("isValidLong") = forAll { (l: Long) => BigInt(l).isValidLong }
  property("isValidLong") = !BigInt("9223372036854775808").isValidLong
  property("isValidLong") = !BigInt("-9223372036854775809").isValidLong
  property("isWhole") = forAll { (bi: BigInt) => bi.isWhole }
  property("underlying") = forAll(bigInteger) { bi => BigInt(bi).underlying ?= bi }
  property("equals") = forAll(bigInteger, bigInteger) { (x, y) => (x == y) ?= (BigInt(x) equals BigInt(y)) }
  property("compare") = forAll(bigInteger, bigInteger) { (x, y) => x.compareTo(y) ?= BigInt(x).compare(y) }
  property("+") = forAll(bigInteger, bigInteger) { (x, y) => x.add(y) ?= (BigInt(x) + BigInt(y)).bigInteger }
  property("-") = forAll(bigInteger, bigInteger) { (x, y) => x.subtract(y) ?= (BigInt(x) - BigInt(y)).bigInteger }
  property("*") = forAll(bigInteger, bigInteger) { (x, y) => x.multiply(y) ?= (BigInt(x) * BigInt(y)).bigInteger }
  property("/") = forAll(bigInteger, nonZeroBigInteger) { (x, y) => x.divide(y) ?= (BigInt(x) / BigInt(y)).bigInteger }
  property("%") = forAll(bigInteger, nonZeroBigInteger) { (x, y) => x.remainder(y) ?= (BigInt(x) % BigInt(y)).bigInteger }
  property("/%") = forAll(bigInteger, nonZeroBigInteger) { (x, y) =>
    val Array(d1, r1) = x.divideAndRemainder(y)
    val (d2, r2) = BigInt(x) /% BigInt(y)
    (d1 ?= d2.bigInteger) && (r1 ?= r2.bigInteger)
  }
  property("<<") = forAll(bigInteger, shift) { (x, s) => x.shiftLeft(s) ?= (BigInt(x) << s).bigInteger }
  property(">>") = forAll(bigInteger, shift) { (x, s) => x.shiftRight(s) ?= (BigInt(x) >> s).bigInteger }
  property("&") = forAll(bigInteger, bigInteger) { (x, y) => x.and(y) ?= (BigInt(x) & BigInt(y)).bigInteger }
  property("|") = forAll(bigInteger, bigInteger) { (x, y) => x.or(y) ?= (BigInt(x) | BigInt(y)).bigInteger }
  property("^") = forAll(bigInteger, bigInteger) { (x, y) => x.xor(y) ?= (BigInt(x) ^ BigInt(y)).bigInteger }
  property("&~") = forAll(bigInteger, bigInteger) { (x, y) => x.andNot(y) ?= (BigInt(x) &~ BigInt(y)).bigInteger }
  property("gcd") = forAll(bigInteger, bigInteger) { (x, y) => x.gcd(y) ?= (BigInt(x) gcd BigInt(y)).bigInteger }
  property("mod") = forAll(bigInteger, positiveBigInteger) { (x, y) => x.mod(y) ?= (BigInt(x) mod BigInt(y)).bigInteger }
  property("min") = forAll(bigInteger, bigInteger) { (x, y) => x.min(y) ?= (BigInt(x) min BigInt(y)).bigInteger }
  property("max") = forAll(bigInteger, bigInteger) { (x, y) => x.max(y) ?= (BigInt(x) max BigInt(y)).bigInteger }
  property("pow") = forAll(bigInteger, smallExponent) { (x, e) => x.pow(e) ?= (BigInt(x) pow e).bigInteger }
  property("modPow") = forAll(bigInteger, positiveBigInteger, positiveBigInteger) { (x, m, e) => x.modPow(e, m) ?= (BigInt(x).modPow(BigInt(e), BigInt(m))).bigInteger }
  property("modPow") = forAll(relativelyPrimePositiveBigIntegers, bigInteger) { case ((x, m), e) => x.modPow(e, m) ?= (BigInt(x).modPow(BigInt(e), BigInt(m))).bigInteger }
  property("modInteger") = forAll(relativelyPrimePositiveBigIntegers) { case (x, m) => x.modInverse(m) ?= BigInt(x).modInverse(BigInt(m)).bigInteger }
  property("unary_-") = forAll(bigInteger) { x => x.negate ?= (-BigInt(x)).bigInteger }
  property("abs") = forAll(bigInteger) { x => x.abs ?= BigInt(x).abs.bigInteger }
  property("signum") = forAll(bigInteger) { x => x.signum ?= BigInt(x).signum }
  property("sign") = forAll(bigInteger) { x => BigInt(x.signum) ?= BigInt(x).sign }
  property("~") = forAll(bigInteger) { x => x.not ?= (~BigInt(x)).bigInteger }
  property("testBit") = forAll(bigInteger, bitIndex) { (x, i) => x.testBit(i) ?= BigInt(x).testBit(i) }
  property("setBit") = forAll(bigInteger, bitIndex) { (x, i) => x.setBit(i) ?= BigInt(x).setBit(i).bigInteger }
  property("clearBit") = forAll(bigInteger, bitIndex) { (x, i) => x.clearBit(i) ?= BigInt(x).clearBit(i).bigInteger }
  property("flipBit") = forAll(bigInteger, bitIndex) { (x, i) => x.flipBit(i) ?= BigInt(x).flipBit(i).bigInteger }
  property("lowestSetBit") = forAll(bigInteger) { x => x.getLowestSetBit ?= BigInt(x).lowestSetBit }
  property("bitLength") = forAll(bigInteger) { x => x.bitLength ?= BigInt(x).bitLength }
  property("bitCount") = forAll(bigInteger) { x => x.bitCount ?= BigInt(x).bitCount }
  property("byteValue") = forAll(bigInteger) { x => x.byteValue ?= BigInt(x).byteValue }
  property("shortValue") = forAll(bigInteger) { x => x.shortValue ?= BigInt(x).shortValue }
  property("charValue") = forAll(bigInteger) { x => x.intValue.toChar ?= BigInt(x).charValue }
  property("intValue") = forAll(bigInteger) { x => x.intValue ?= BigInt(x).intValue }
  property("longValue") = forAll(bigInteger) { x => x.longValue ?= BigInt(x).longValue }
  property("floatValue") = forAll(bigInteger) { x => x.floatValue ?= BigInt(x).floatValue }
  property("doubleValue") = forAll(bigInteger) { x => x.doubleValue ?= BigInt(x).doubleValue }
  property("toString") = forAll(bigInteger) { bi => new BigInteger(BigInt(bi).toString) ?= bi }
  property("toString(radix: Int)") = forAll(bigInt, radix) { (bi, r) => BigInt(bi.toString(r), r) ?= bi }
  property("toByteArray") = forAll(bigInt) { bi => BigInt(bi.toByteArray) ?= bi }
}
