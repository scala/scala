package subscript.test
import scala.language.implicitConversions
import scala.swing._
import scala.swing.event._
import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._ //tweede keer geimporteerd!!!!!!!!!
import subscript.vm._

// Subscript sample application: a parallel recursive implementation of a Bag
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala
//

object Bag extends BagApplication

class BagApplication extends SimpleSubscriptApplication {
  
  val lA = new Label("A") {preferredSize = new Dimension(26,26)}
  val lB = new Label("B") {preferredSize = new Dimension(26,26)}
  val pA = new Button("+")          {enabled       = false}
  val pB = new Button("+")          {enabled       = false}
  val mA = new Button("-")          {enabled       = false}
  val mB = new Button("-")          {enabled       = false}
  val cA = new Label("")  {preferredSize = new Dimension(45,26)}
  val cB = new Label("")  {preferredSize = new Dimension(45,26)}
  val  X = new Button("Exit")       {enabled       = false}
  val bagLabel = new Label("Bag") {preferredSize = new Dimension(45,26)}
  val outputTA = new TextArea      {editable      = false}
  
  
  val top          = new MainFrame {
    title          = "Bag - Subscript"
    location       = new Point    (0,0)
    preferredSize  = new Dimension(300,300)
    contents       = new BorderPanel {
      //add(new FlowPanel(bagLabel, X), BorderPanel.Position.North) 
      add(new FlowPanel(lA, pA, mA, cA), BorderPanel.Position.North) 
      add(new FlowPanel(lB, pB, mB, cB), BorderPanel.Position.Center) 
      add(outputTA, BorderPanel.Position.South) 
    }
  }
  var ca = 0
  var cb = 0
  def dA(d: Int) = {ca+=d; cA.text = ca.toString}
  def dB(d: Int) = {cb+=d; cB.text = cb.toString}
  override def  live = _execute(_live())

  override def script
           live = bag
  def script ..
            bag = A (bag&ax)
                + B (bag&bx)
                
            A   = pA @{gui(there)}:{!dA(+1)!}
            ax  = mA @{gui(there)}:{!dA(-1)!}
            B   = pB @{gui(there)}:{!dB(+1)!}
            bx  = mB @{gui(there)}:{!dB(-1)!}
}
