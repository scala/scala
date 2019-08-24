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
//  def optimize(b: Boolean): Int = (b: @exhaustive) match {
//    case true  => 10
//    case false => 11
//  }

  //For comparison
//  def optimize(b: Boolean): Int = b match {
//    case true  => 10
//    case _ => 11 //This is a BodyTreeMaker instead. Maybe the trick is to make the above a BodyTreeMaker if exhaustive and we have the annotation
//  }
//
//  optimize(false)

//  sealed trait Woah
//  case object Woah1 extends Woah
//  case object Woah2 extends Woah
//
//  def optimize2(w: Woah): Int = (w: @exhaustive) match {
//    case Woah1 => 9
//    case Woah2 => 3
//  }

//  optimize2(Woah1)

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
