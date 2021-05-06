package scala.collection

import scala.util.Try
import org.scalacheck.{Arbitrary, Gen, Properties, Shrink}
import org.scalacheck.Test.Parameters
import org.scalacheck.Prop._

object FloatFormatTest extends Properties("FloatFormat") {

  import StringParsers._

  val genDec = for {
    sign <- Gen.oneOf("", "+", "-")
    int <- Gen.numStr
    dot <- Gen.oneOf("", ".")
    frac <- Gen.numStr
  } yield sign + int + dot + frac

  val genHexChar = Gen.oneOf(
    Gen.numChar,
    Gen.choose('a', 'f'),
    Gen.choose('A', 'F')
  )

  val genHexString = for {
    hexstring <- Gen.listOf(genHexChar).map(_.mkString)
    prefix <- Gen.oneOf("0x", "0X")
  } yield prefix + hexstring

  val genHex = for {
    sign <- Gen.oneOf("", "+", "-")
    int <- genHexString
    dot <- Gen.oneOf("", ".")
    frac <- genHexString
  } yield sign + int + dot + frac

  val genDecOrHex = Gen.oneOf(genDec, genHex)

  val genWSChar = Gen.const(' ')

  val genWS = Gen.listOf(genWSChar).map(_.mkString)

  val genExpSign = Gen.oneOf("", "e", "E", "p", "P")

  val genSuffix = Gen.oneOf("", "f", "F", "d", "D")

  val anystring: Gen[String] = Arbitrary.arbString.arbitrary

  val genPossibleFloatString = {
    val parts = List(genWS, genDecOrHex, genExpSign, genDecOrHex, genSuffix, genWS)
    genBogifiedString(parts)
  }

  val genNanInf = for {
    ws <- genWS
    signum <- Gen.oneOf("", "+", "-")
    nanInf <- Gen.oneOf("NaN", "Infinity")
    wse <- genWS
  } yield  ws + signum + nanInf + wse

  val genPlausibleFloatString = for {
    ws <- genWS
    dec <- genDecOrHex
    exp <- genExpSign
    dec2 <- genDecOrHex
    suffix <- genSuffix
    wse <- genWS
  } yield ws + dec + exp + dec2 + suffix + wse

  def genBogifiedString(parts: List[Gen[String]]): Gen[String] = {
    val wrong: Gen[String] = {
      parts match {
        case head :: tail => Gen.oneOf(anystring, head, tail :_*)
        case Nil => anystring
      }
    }
    
    val bogoparts: List[Gen[String]] = parts.map(right =>
      Gen.frequency(
        1 -> wrong,
        10 -> right
      ))
    
    import scala.jdk.CollectionConverters._
    Gen.sequence(bogoparts).map(_.asScala.mkString)
  }

  //compare NaN equal
  def deq(l: Option[Double], r: Option[Double]): Boolean = (l, r) match {
    case (Some(ll), Some(rr)) if ll.isNaN && rr.isNaN => true
    case (ll, rr) => ll == rr
  }

  property("for all strings (plausible examples) str.toDoubleOption == Try(str.toDouble).toOption") = forAll(genPlausibleFloatString) {
    str => deq(str.toDoubleOption, Try(str.toDouble).toOption)
  }

  property("for all strings (possible examples) str.toDoubleOption == Try(str.toDouble).toOption") = forAll(genPossibleFloatString) { 
    str => deq(str.toDoubleOption, Try(str.toDouble).toOption)
  }

  property("for all strings (random strings) str.toDoubleOption == Try(str.toDouble).toOption") = forAll{ (str: String) =>
    deq(str.toDoubleOption, Try(str.toDouble).toOption)
  }

  property("double.toString is a valid float format") = forAll{ (d: Double) => checkFloatFormat(d.toString)}

  property("nan and infinity toDouble == toDoubleOption.get") = forAll(genNanInf){ (str: String) =>
    deq(str.toDoubleOption, Try(str.toDouble).toOption)
  }

}
