import scala.xml._
import scala.xml.pull._
import scala.io.Source

object Test {

  val src = Source.fromString("<hello><world/>!</hello>")
 
  def main(args: Array[String]) {
    var er = new XMLEventReader(src)
    er.next match {
      case EvElemStart(_, "hello", _, _) => //println("1")
    }
    er.next match {
      case EvElemStart(_, "world", _, _) => //println("2")
    }
    er.next match {
      case EvElemEnd(_, "world") => //println("3")
    }
    er.next match {
      case EvText("!") => //println("4")
    }
    er.next match {
      case EvElemEnd(_, "hello") => //println("5")
    }
    // you get the picture...
    er.stop  // allow thread to be garbage-collected
    //println("6")
  }
}
 
