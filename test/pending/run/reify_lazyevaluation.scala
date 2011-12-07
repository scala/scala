import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    object lazyLib {

      /** Delay the evaluation of an expression until it is needed. */
      def delay[A](value: => A): Susp[A] = new SuspImpl[A](value)

      /** Get the value of a delayed expression. */
      implicit def force[A](s: Susp[A]): A = s()

      /**
       * Data type of suspended computations. (The name froms from ML.)
       */
      abstract class Susp[+A] extends Function0[A]

      /**
       * Implementation of suspended computations, separated from the
       * abstract class so that the type parameter can be invariant.
       */
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

        override def toString() = maybeValue match {
          case None => "Susp(?)"
          case Some(value) => "Susp(" + value + ")"
        }
      }
    }

    import lazyLib._

    val s: Susp[Int] = delay { println("evaluating..."); 3 }

    println("s     = " + s)       // show that s is unevaluated
    println("s()   = " + s())     // evaluate s
    println("s     = " + s)       // show that the value is saved
    println("2 + s = " + (2 + s)) // implicit call to force()

    val sl = delay { Some(3) }
    val sl1: Susp[Some[Int]] = sl
    val sl2: Susp[Option[Int]] = sl1   // the type is covariant

    println("sl2   = " + sl2)
    println("sl2() = " + sl2())
    println("sl2   = " + sl2)
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
