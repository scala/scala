abstract class CharSet[P] {
  type Type <: P
}

object LetterOrDigit extends CharSet[Char]
object Digit extends CharSet[LetterOrDigit.Type]

object t {
  type D = Digit.Type
}