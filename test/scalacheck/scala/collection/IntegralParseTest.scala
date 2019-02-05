package scala.collection

import scala.util.Try
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Shrink
import org.scalacheck.Test.Parameters
import org.scalacheck.Prop._

import org.scalacheck.Gen

object NumericStringGenerators {

  val nearOverflowByteUpper = {
    val lower: Int = Byte.MaxValue.toInt / 10
    val upper: Int = Byte.MaxValue.toInt * 20
    Gen.choose(lower, upper).map(_.toString)
  }

  val nearOverflowByteLower = {
    val upper: Int = Byte.MinValue.toInt / 10
    val lower: Int = Byte.MinValue.toInt * 20
    Gen.choose(lower, upper).map(_.toString)
  }

  val nearOverflowShortUpper = {
    val lower: Int = Short.MaxValue.toInt / 10
    val upper: Int = Short.MaxValue.toInt * 20
    Gen.choose(lower, upper).map(_.toString)
  }


  val nearOverflowShortLower = {
    val upper: Int = Short.MinValue.toInt / 10
    val lower: Int = Short.MinValue.toInt * 20
    Gen.choose(lower, upper).map(_.toString)
  }

  val nearOverflowIntUpper = {
    val lower: Long = Int.MaxValue.toLong / 10
    val upper: Long = Int.MaxValue.toLong * 20
    Gen.choose(lower, upper).map(_.toString)
  }

  val nearOverflowIntLower = {
    val upper: Long = Int.MinValue.toLong / 10
    val lower: Long = Int.MinValue.toLong * 20
    Gen.choose(lower, upper).map(_.toString)
  }

  val nearOverflowLongUpper = {
    val base = (Long.MaxValue / 10).toString
    for {
      end <- Gen.choose(0, 200)
    } yield base + end.toString
  }

  val nearOverflowLongLower = {
    val base = (Long.MinValue / 10).toString
    for {
      end <- Gen.choose(0, 200)
    } yield base + end.toString
  }

  val overflowByteUpper = Gen.choose(Byte.MaxValue.toInt + 1, Short.MaxValue.toInt).map(_.toString)
  val overflowByteLower = Gen.choose(Short.MinValue.toInt, Byte.MinValue.toInt - 1).map(_.toString)
  val overflowShortUpper = Gen.choose(Short.MaxValue.toInt + 1, Int.MaxValue).map(_.toString)
  val overflowShortLower = Gen.choose(Int.MinValue, Short.MinValue.toInt - 1).map(_.toString)
  val overflowIntUpper = Gen.choose(Int.MaxValue.toLong + 1, Long.MaxValue).map(_.toString)
  val overflowIntLower = Gen.choose(Long.MinValue, Int.MinValue.toLong - 1).map(_.toString)

  val validBytes = 
    for {
      byte <- Gen.choose(Byte.MinValue, Byte.MaxValue)
      zeroes <- Gen.listOf(Gen.const('0')).map(_.mkString)
      signum <- if (byte > 0) Gen.oneOf(List("+", "")) else if (byte == 0) Gen.oneOf(List("+", "", "-")) else Gen.const("-")
    } yield {
      val numbers = if (byte >= 0) byte.toString else byte.toString.substring(1)
      signum + zeroes + numbers
    }

  val validShorts = 
    for {
      short <- Gen.choose(Short.MinValue, Short.MaxValue)
      zeroes <- Gen.listOf(Gen.const('0')).map(_.mkString)
      signum <- if (short > 0) Gen.oneOf(List("+", "")) else if (short == 0) Gen.oneOf(List("+", "", "-")) else Gen.const("-")
    } yield {
      val numbers = if (short >= 0) short.toString else short.toString.substring(1)
      signum + zeroes + numbers
    }

  val validInts = 
    for {
      int <- Gen.choose(Int.MinValue, Int.MaxValue)
      zeroes <- Gen.listOf(Gen.const('0')).map(_.mkString)
      signum <-if (int > 0) Gen.oneOf(List("+", "")) else if (int == 0) Gen.oneOf(List("+", "", "-")) else Gen.const("-")
    } yield {
      val numbers = if (int >= 0) int.toString else int.toString.substring(1)
      signum + zeroes + numbers
    }

  val validLongs = 
    for {
      long <- Gen.choose(Long.MinValue, Long.MaxValue)
      zeroes <- Gen.listOf(Gen.const('0')).map(_.mkString)
      signum <- if (long > 0) Gen.oneOf(List("+", "")) else if (long == 0) Gen.oneOf(List("+", "", "-")) else Gen.const("-")
    } yield {
      val numbers = if (long >= 0) long.toString else long.toString.substring(1)
      signum + zeroes + numbers
    }
    

  val digitsByValue = {
    val allnumbers = (Char.MinValue.toChar to Char.MaxValue.toChar).filter(ch => java.lang.Character.isDigit(ch))
    allnumbers.groupBy(n => java.lang.Character.digit(n, 10))
  }

  def nonStandardNumbers(numeric: String) = {
    val listOfGens: List[Gen[Char]] = numeric.toList.map(ch => {
      val n = java.lang.Character.digit(ch, 10)
      if (n >= 0) Gen.oneOf(digitsByValue(n))
      else Gen.const(ch)
    })
    val sequenced = Gen.sequence(listOfGens)
    sequenced.map(l => scala.collection.JavaConverters.asScalaBuffer(l).mkString)
  }

}

object IntegralParseTest extends Properties("ParseX") {


  implicit val noShrink: Shrink[String] = Shrink.shrinkAny
  import NumericStringGenerators._

  //Byte
  property("nearUpperByte") = forAll(nearOverflowByteUpper){ 
    bytestring: String => bytestring.toByteOption == Try(java.lang.Byte.parseByte(bytestring)).toOption
  }

  property("nearUpperByteNonstandard") = forAll(nearOverflowByteUpper.flatMap(nonStandardNumbers)){ 
    bytestring: String => bytestring.toByteOption == Try(java.lang.Byte.parseByte(bytestring)).toOption
  }

  property("nearLowerByte") = forAll(nearOverflowByteLower){ 
    bytestring: String => bytestring.toByteOption == Try(java.lang.Byte.parseByte(bytestring)).toOption
  }

  property("nearLowerByteNonstandard") = forAll(nearOverflowByteLower.flatMap(nonStandardNumbers)){ 
    bytestring: String => bytestring.toByteOption == Try(java.lang.Byte.parseByte(bytestring)).toOption
  }

  property("overflowByteUpper") = forAll(overflowByteUpper){ 
    bytestring: String => bytestring.toByteOption == None
  }

  property("overflowByteLower") = forAll(overflowByteLower){ 
    bytestring: String => bytestring.toByteOption == None
  }

  property("validBytes") = forAll(validBytes){
    bytestring: String => {
      val parsed = bytestring.toByteOption
      parsed.isDefined &&
      parsed == Some(java.lang.Byte.parseByte(bytestring))
    }
  }

  property("validBytesNonstandard") = forAll(validBytes.flatMap(nonStandardNumbers)){
    bytestring: String => {
      val parsed = bytestring.toByteOption
      parsed.isDefined &&
      parsed == Some(java.lang.Byte.parseByte(bytestring))
    }
  }

  //Short

  property("nearUpperShort") = forAll(nearOverflowShortUpper){ 
    shortstring: String => shortstring.toShortOption == Try(java.lang.Short.parseShort(shortstring)).toOption
  }

  property("nearUpperShortNonstandard") = forAll(nearOverflowShortUpper.flatMap(nonStandardNumbers)){ 
    shortstring: String => shortstring.toShortOption == Try(java.lang.Short.parseShort(shortstring)).toOption
  }

  property("nearLowerShort") = forAll(nearOverflowShortLower){ 
    shortstring: String => shortstring.toShortOption == Try(java.lang.Short.parseShort(shortstring)).toOption
  }

  property("nearLowerShortNonstandard") = forAll(nearOverflowShortLower.flatMap(nonStandardNumbers)){ 
    shortstring: String => shortstring.toShortOption == Try(java.lang.Short.parseShort(shortstring)).toOption
  }

  property("overflowShortUpper") = forAll(overflowShortUpper){ 
    shortstring: String => shortstring.toShortOption == None
  }

  property("overflowShortLower") = forAll(overflowShortLower){ 
    shortstring: String => shortstring.toShortOption == None
  }

  property("validShorts") = forAll(validShorts){
    shortstring: String => {
      val parsed = shortstring.toShortOption
      parsed.isDefined &&
      parsed == Some(java.lang.Short.parseShort(shortstring))
    }
  }

  property("validShortsNonstandard") = forAll(validShorts.flatMap(nonStandardNumbers)){
    shortstring: String => {
      val parsed = shortstring.toShortOption
      parsed.isDefined &&
      parsed == Some(java.lang.Short.parseShort(shortstring))
    }
  }

  //Int

  property("nearUpperInt") = forAll(nearOverflowIntUpper){ 
    intstring: String => intstring.toIntOption == Try(java.lang.Integer.parseInt(intstring)).toOption
  }

  property("nearUpperIntNonstandard") = forAll(nearOverflowIntUpper.flatMap(nonStandardNumbers)){ 
    intstring: String => intstring.toIntOption == Try(java.lang.Integer.parseInt(intstring)).toOption
  }

  property("nearLowerInt") = forAll(nearOverflowIntLower){ 
    intstring: String => intstring.toIntOption == Try(java.lang.Integer.parseInt(intstring)).toOption
  }

  property("nearLowerIntNonstandard") = forAll(nearOverflowIntLower.flatMap(nonStandardNumbers)){ 
    intstring: String => intstring.toIntOption == Try(java.lang.Integer.parseInt(intstring)).toOption
  }

  property("overflowIntUpper") = forAll(overflowIntUpper){ 
    intstring: String => intstring.toIntOption == None
  }

  property("overflowIntLower") = forAll(overflowIntLower){ 
    intstring: String => intstring.toIntOption == None
  }

  property("validInts") = forAll(validInts){
    intstring: String => {
      val parsed = intstring.toIntOption
      parsed.isDefined &&
      parsed == Some(java.lang.Integer.parseInt(intstring))
    }
  }

  property("validIntsNonstandard") = forAll(validInts.flatMap(nonStandardNumbers)){
    intstring: String => {
      val parsed = intstring.toIntOption
      parsed.isDefined &&
      parsed == Some(java.lang.Integer.parseInt(intstring))
    }
  }

  //long
  property("nearUpperLong") = forAll(nearOverflowLongUpper){ 
    longstring: String => longstring.toLongOption == Try(java.lang.Long.parseLong(longstring)).toOption
  }

  property("nearUpperLongNonstandard") = forAll(nearOverflowLongUpper.flatMap(nonStandardNumbers)){ 
    longstring: String => longstring.toLongOption == Try(java.lang.Long.parseLong(longstring)).toOption
  }

  property("nearLowerLong") = forAll(nearOverflowLongLower){ 
    longstring: String => longstring.toLongOption == Try(java.lang.Long.parseLong(longstring)).toOption
  }

  property("nearLowerLongNonstandard") = forAll(nearOverflowLongLower.flatMap(nonStandardNumbers)){ 
    longstring: String => longstring.toLongOption == Try(java.lang.Long.parseLong(longstring)).toOption
  }

  property("validLongs") = forAll(validLongs){
    longstring: String => {
      val parsed = longstring.toLongOption
      parsed.isDefined &&
      parsed == Some(java.lang.Long.parseLong(longstring))
    }
  }

  property("validLongsNonstandard") = forAll(validLongs.flatMap(nonStandardNumbers)){
    longstring: String => {
      val parsed = longstring.toLongOption
      parsed.isDefined &&
      parsed == Some(java.lang.Long.parseLong(longstring))
    }
  }

} 
