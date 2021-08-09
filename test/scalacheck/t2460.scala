import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.{Test => SCTest}
import org.scalacheck.Gen

object SI2460Test extends Properties("Regex : Ticket 2460") {

  val vowel = Gen.oneOf("a", "z")

  val numberOfMatch = forAll(vowel) {
    (s: String) => "\\s*([a-z])\\s*".r.findAllMatchIn((1 to 20).map(_ => s).mkString).size == 20
  }

  val numberOfGroup = forAll(vowel) {
    (s: String) => "\\s*([a-z])\\s*([a-z])\\s*".r.findAllMatchIn((1 to 20).map(_ => s).mkString).next().groupCount == 2
  }

  val nameOfGroup = forAll(vowel) {
    (s: String) => "(?<data>[a-z])".r.findAllMatchIn(s).next().group("data") == s
  }

  val tests = List(
    ("numberOfMatch", numberOfMatch),
    ("numberOfGroup", numberOfGroup),
    ("nameOfGroup", nameOfGroup)
  )
}
