package tastytest

object Intersections {
  trait A
  trait B

  type AwithB = A with B
  type AandB = A & B

  abstract class Printer[-A] {
    def println(a: A): String
  }

  object AwithBPrinter extends Printer[AwithB] {
    def println(a: AwithB): String = "AwithB"
  }

  object AandBPrinter extends Printer[A & B] {
    def println(a: A & B): String = "A & B"
  }
}
