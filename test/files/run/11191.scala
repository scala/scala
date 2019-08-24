import scala.annotation.exhaustive
import scala.tools.partest._
import java.io.{Console => _, _}


object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Vprint:patmat -Vprint-types -d " + testOutput.path

  override def code = """class C {
  def optimizeDefault(b: Boolean): Int = b match {
        case true  => 10
        case false => 11
      }
  }
  """

// Just some comments that will be removed. Feel free to ignore.
//For comparison
//  def optimize(b: Boolean): Int = b match {
//    case true  => 10
//    case _ => 11 //This is a BodyTreeMaker instead. Maybe the trick is to make the above a BodyTreeMaker if ok? Ahh I see how it works. The first thing is the condition just drop the first thing.
//  }
//
//  optimize(false)

// Would not actually do this since it's a Boolean. Just need to figure out how to know if @exhaustive is present in optimizeCase.
// TODO would I even want to use @exhaustive in this way. This is how @switch is used.
//  def optimize(b: Boolean): Int = (b: @exhaustive) match {
//    case true  => 10
//    case false => 11
//  }

// This should also optimize by default
//  sealed trait Woah
//  case object Woah1 extends Woah
//  case object Woah2 extends Woah
//
//  def optimize2(w: Woah): Int = w match {
//    case Woah1 => 9
//    case Woah2 => 3
//  }
//  optimize2(Woah1)


// Don't optimize by default since we don't know if something else extends Woah
// But do optimize in this case since the @exhaustive annotation is present
//    trait Woah
//    case object Woah1 extends Woah
//    case object Woah2 extends Woah
//
//    def optimize2(w: Woah): Int = (w: @exhaustive match {
//      case Woah1 => 9
//      case Woah2 => 3
//    }
//    optimize2(Woah1)

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
