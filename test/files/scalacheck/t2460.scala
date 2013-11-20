import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.{Test => SCTest}
import org.scalacheck.Gen

object Test extends Properties("Regex : Ticket 2460") {

  val vowel = Gen.oneOf("a", "z")

  val numberOfMatch = forAll(vowel) {
    (s: String) => "\\s*([a-z])\\s*".r("data").findAllMatchIn((1 to 20).map(_ => s).mkString).size == 20
  }

  val numberOfGroup = forAll(vowel) {
    (s: String) => "\\s*([a-z])\\s*([a-z])\\s*".r("data").findAllMatchIn((1 to 20).map(_ => s).mkString).next.groupCount == 2
  }

  val nameOfGroup = forAll(vowel) {
    (s: String) => "([a-z])".r("data").findAllMatchIn(s).next.group("data") == s
  }

  val tests = List(
    ("numberOfMatch", numberOfMatch),
    ("numberOfGroup", numberOfGroup),
    ("nameOfGroup", nameOfGroup)
  )
}
