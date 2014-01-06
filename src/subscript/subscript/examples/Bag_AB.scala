import scala.language.implicitConversions
import scala.swing._
import scala.swing.event._
import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._

// Subscript sample application: A..B
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala

abstract class Bag_AB_Application extends SimpleSubscriptApplication {
  def getTitle: String
  
  val A = new Button("A") {enabled = false}
  val B = new Button("B") {enabled = false}
  val a = new Button("a") {enabled = false}
  val b = new Button("b") {enabled = false}
  
  val top          = new MainFrame {
    title          = getTitle
    location       = new Point    (800,0)
    preferredSize  = new Dimension(400,70)
    contents       = new BorderPanel {
      add(new FlowPanel(A, B, a, b), BorderPanel.Position.North) 
    }
  }
  override def live = _execute(_live())
  def script live
}

object Bag_A extends Bag_AB_Application {
  def getTitle = "Bag: live = A (live&a)"

 override def script..
   live = A (live&a) 
}

object Bag_AB extends Bag_AB_Application {
  def getTitle = "Bag: live = A (live&a) + B (live&b)"

 override def script..
   live = A (live&a) 
        + B (live&b)
}

object Bag_AB_loops extends Bag_AB_Application {
  def getTitle = "Bag = (A a & ..) & (B b & ..)"

 override def script..
   live = ( A a & .. )
        & ( B b & .. )
}

