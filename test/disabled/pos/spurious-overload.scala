object Test extends App {
  def foo(bar: Any) = bar

  val code = foo{
    object lazyLib {

      def delay[A](value: => A): Susp[A] = new SuspImpl[A](value)

      implicit def force[A](s: Susp[A]): A = s()

      abstract class Susp[+A] extends Function0[A]

      class SuspImpl[A](lazyValue: => A) extends Susp[A] {
        private var maybeValue: Option[A] = None

        override def apply() = maybeValue match {
          case None =>
            val value = lazyValue
            maybeValue = Some(value)
            value
          case Some(value) =>
            value
        }
      }
    }

    import lazyLib._

    val s: Susp[Int] = delay { println("evaluating..."); 3 }
    println("2 + s = " + (2 + s)) // implicit call to force()
  }
}