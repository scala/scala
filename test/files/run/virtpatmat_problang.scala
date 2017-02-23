trait ProbCore {
  type Prob = Double
  case class RandVar[+A](val dist: List[(A,Prob)]) {
    def flatMap[B](f: A => RandVar[B]): RandVar[B] =
      RandVar(dist.flatMap{case (x,p) => f(x).factor(p).dist})
    def map[B](f: A => B): RandVar[B] =
      RandVar(dist.map{case (x,p) => (f(x),p)})
    // NOTE(namin): orElse is overcounting if the cases are not disjoint! See roulete payoffs.
    // How can we fix this? And do we want to?
    // P(A \/ B) = P(A) + P(B) - P(A /\ B) <-- can we get P(A /\ B)?
    def orElse[B >: A](that: RandVar[B]) =
      RandVar(this.dist ++ that.dist)
    def weight =
      dist.map(_._2).sum
    def factor(w: Double): RandVar[A] =
      RandVar(dist.map{case (x,p) => (x,p*w)})
    def consolidate: RandVar[A] =
      RandVar(dist.filter(_._2 > 0).groupBy(_._1).map{case (x,ps) => (x, ps.map(_._2).sum)}.toList.sortBy{case (x,p) => (-p,x.toString)})
    def normalize: RandVar[A] = {
      val r = flatten.consolidate
      r.factor(1.0/r.weight)
    }
    def flatten: RandVar[A] =
      RandVar(dist.flatMap{case (x,p) => x match {
	  case r @ RandVar(_) => r.flatten.factor(p).dist
	  case _ => List((x,p))
      }}).asInstanceOf[RandVar[A]]
  }

  def boolFlip(p: Double) = RandVar(List((true, p), (false, 1-p)))
  def always[A](x: A) = RandVar(List((x, 1.0)))
  val never = RandVar(Nil)

  def weightedCases[A](inp: (A,Prob)*) = RandVar(inp.toList)
  def countedCases[A](inp: (A, Int)*) = {
    val total = 1.0*inp.map(_._2).sum
    weightedCases(inp map { case (x,c) => (x,c/total) }:_*)
  }
}

trait ProbLift extends ProbCore {
  import scala.language.implicitConversions
  implicit def liftRandVar[T](x: T): RandVar[T] = always(x)
}

trait ProbCond extends ProbCore with EmbeddedControls {
  def __ifThenElse[T](cond: => RandVar[Boolean], thenp: => RandVar[T], elsep: => RandVar[T]): RandVar[T] =
    cond.flatMap(c => c match {
      case true => thenp
      case false => elsep
    })
}

trait ProbMatcher extends ProbCore with ProbLift {
  object __match {
    def one[T](x: T): RandVar[T] =
      always(x)
    def zero = never
    def guard[T](cond: Boolean, result: => T): RandVar[T] =
      if (cond) one(result) else zero
    def runOrElse[T, U](in: T)(matcher: T => RandVar[U]): RandVar[U] =
      matcher(in).flatten
  }
}

trait ProbExtractor extends ProbMatcher {
  trait ValueExtractor {
    type A
    val value: A
    def unapply(that: A): RandVar[A] = if (value == that) always(value) else never
    def unapply(r: RandVar[A]): RandVar[A] = r.flatMap(unapply)
  }
  class CaseExtractor(desc: String) extends ValueExtractor {
    type A >: this.type
    val value: A = this
    override def toString = desc
  }
  def ValueExtractor[T](x: T) = new ValueExtractor {
    type A = T
    val value: A = x
  }
  val True = ValueExtractor[Boolean](true)
  val False = ValueExtractor[Boolean](false)
}

trait ProbIndicator extends ProbMatcher {
  object Indicator {
    def unapply[A](r: RandVar[A]): RandVar[A] = r
  }
}

trait ProbLang extends ProbCore with ProbCond with ProbMatcher with ProbExtractor with ProbIndicator

trait ProbPrettyPrint extends ProbCore {
  def pp[A](r: RandVar[A]) = r.dist.map{case (x,p) => x + " : " + p}.mkString("\n")
  def show[A](r: RandVar[A], desc: String = "") = {
    println(desc)
    println(pp(r.normalize))
    println("")
  }
}

trait ProbCondEx extends ProbCond with ProbLift {
  val cond1 = {
    val x = boolFlip(0.5)
    val y = if (x) 1 else if (x) 2 else 3
    y
  }
}

trait ProbMatcherExRoulette extends ProbMatcher with ProbExtractor {
  object RouletteModel {
    sealed class Outcome(name: String) extends CaseExtractor(name) { type A = Outcome }
    object Even extends Outcome("Even")
    object Odd extends Outcome("Odd")
    object Zero extends Outcome("Zero")
  }
  import RouletteModel._

  val roulette = countedCases(Even -> 18, Odd -> 18, Zero -> 1)

  val roulette1 = roulette match {
    case r => r
  }

  val roulettePayoff = roulette match {
    case Even(_) => 10
    case Odd(_) => 0
    case Zero(_) => 0
  }

  val roulettePayoff1 = roulette match {
    case r => r match {
      case Even(_) => 10
      case Odd(_) => 0
      case Zero(_) => 0
    }
  }

  val roulettePayoff2 = roulette match {
    case Even(_) => 10
    case _ => 0
  }
  val roulettePayoff3 = roulette match {
    case Even(_) => 10
    case Odd(_) => 1
    case Zero(_) => 1
  }
  val roulettePayoff4 = roulette match {
    case Even(_) => 10
    case _ => 1
  }
  val roulettePayoff5 = roulette match {
    case Even(r) => 10
    case Odd(r) => 5
    case Zero(r) => 0
  }
}

trait ProbLangExRoulette extends ProbLang with ProbMatcherExRoulette {
  import RouletteModel._

  val roulettePayoff6 = roulette match {
    case Even(_) => boolFlip(0.5) match {
      case True(_) => 10
      case False(_) => 0
    }
    case Odd(_) => 0
    case Zero(_) => 0
  }

  val roulettePayoff7 = roulette match {
    case Even(_) => 10
  }

  val roulettePayoff8 = roulettePayoff match {
    case Indicator(payoff) => payoff-1
  }
}

trait ProbLangExTraffic extends ProbLang {
  object TrafficModel {
    sealed class Light(name: String) extends CaseExtractor(name) { type A = Light }
    object Red extends Light("Red")
    object Green extends Light("Green")
    object Yellow extends Light("Yellow")

    sealed class Action(name: String) extends CaseExtractor(name) { type A = Action }
    object Stop extends Action("Stop")
    object Drive extends Action("Drive")

    sealed class CrashResult(name: String) extends CaseExtractor(name) { type A = CrashResult }
    object Crash extends CrashResult("Crash")
    object NoCrash extends CrashResult("NoCrash")

    type Driver = RandVar[Light] => RandVar[Action]
  }
  import TrafficModel._

  val trafficLight = weightedCases(Red -> 0.5, Yellow -> 0.1, Green -> 0.4)
  def cautiousDriver(light: RandVar[Light]) = (light match {
    case Red(_) => always(Stop)
    case Yellow(_) => weightedCases(Stop -> 0.9, Drive -> 0.1)
    case Green(_) => always(Drive)
  }).asInstanceOf[RandVar[Action]]
  def aggressiveDriver(light: RandVar[Light]) = (light match {
    case Red(_) => weightedCases(Stop -> 0.9, Drive -> 0.1)
    case Yellow(_) => weightedCases(Stop -> 0.1, Drive -> 0.9)
    case Green(_) => always(Drive)
  }).asInstanceOf[RandVar[Action]]
  def otherLight(light: RandVar[Light]) = light match {
    case Red(_) => Green
    case Yellow(_) => Red
    case Green(_) => Red
  }
  def crash(driver1: Driver, driver2: Driver, light: RandVar[Light]) = light match {
    case Indicator(l1) => otherLight(l1) match {
      case Indicator(l2) => (driver1(l1), driver2(l2)) match {
        case (Indicator(d1), Indicator(d2)) if d1==Drive && d2==Drive =>
          weightedCases(Crash -> 0.9, NoCrash -> 0.1)
        case (Indicator(d1), Indicator(d2)) if d1!=Drive || d2!=Drive =>
          NoCrash
      }
    }
  }
  val trafficModel = crash(cautiousDriver, aggressiveDriver, trafficLight)
  val trafficModel2 = crash(aggressiveDriver, aggressiveDriver, trafficLight)
}

object Test extends App with ProbPrettyPrint with ProbCondEx with ProbMatcherExRoulette with ProbLangExRoulette with ProbLangExTraffic {
  show(cond1, "cond1")

  show(roulette, "roulette")
  show(roulette1, "roulette1")
  show(roulettePayoff, "roulettePayoff")
  show(roulettePayoff1, "roulettePayoff1")
  show(roulettePayoff2, "roulettePayoff2")
  show(roulettePayoff3, "roulettePayoff3")
  show(roulettePayoff4, "roulettePayoff4")
  show(roulettePayoff5, "roulettePayoff5")
  show(roulettePayoff6, "roulettePayoff6")
  show(roulettePayoff7, "roulettePayoff7")
  show(roulettePayoff8, "roulettePayoff8")

  show(trafficModel, "trafficModel")
  show(trafficModel2, "trafficModel2")
}
