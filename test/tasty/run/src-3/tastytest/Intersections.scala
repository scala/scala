package tastytest

object Intersections {
  trait A
  trait B
  trait C
  trait D

  type AwithB = A with B
  type AandB = A & B
  type AandBandCandD = A & B & C & D

  abstract class Printer[-A] {
    def println(a: A): String
  }

  object AwithBPrinter extends Printer[AwithB] {
    def println(a: AwithB): String = "AwithB"
  }

  object AandBPrinter extends Printer[A & B] {
    def println(a: A & B): String = "A & B"
  }

  object AandBandCandDPrinter extends Printer[A & B & C & D] {
    def println(a: A & B & C & D): String = "A & B & C & D"
  }
}
