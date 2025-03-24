//> using options -Xsource:3

//import org.scalacheck._, Prop._

object Main extends App {
  class Prop
  class Gen[A]
  object Gen {
    implicit def const[T](x: T): Gen[T] = ???
  }

  def forAll[T1, P](g: Gen[T1])(f: T1 => P)(implicit p: P => Prop): Prop = ???
  def forAll[A1, P](f: A1 => P)(implicit p: P => Prop): Prop = ???

  def what() = forAll {
    (a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int, a7: Int,
     a8: Int,
     a9: Int,
    ) => false
  }

}

/*
    def what(): (((Int, Int, Int, Int, Int, Int, Int, Int, Int) => Boolean) => Nothing) => Main.Prop = {
            <synthetic> val eta$0$1: Main.Gen[(Int, Int, Int, Int, Int, Int, Int, Int, Int) => Boolean] = Main.this.Gen.const[(Int, Int, Int, Int, Int, Int, Int, Int, Int) => Boolean](((a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int, a7: Int, a8: Int, a9: Int) => false));
                  ((f: ((Int, Int, Int, Int, Int, Int, Int, Int, Int) => Boolean) => Nothing) => Main.this.forAll[(Int, Int, Int, Int, Int, Int, Int, Int, Int) => Boolean, Nothing](eta$0$1)(f)(scala.Predef.$conforms[Nothing]))
*/
