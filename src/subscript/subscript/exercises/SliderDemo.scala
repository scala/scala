package subscript.exercises

import scala.language.implicitConversions
import java.awt.{Graphics, Image, Color, Point}
import java.awt.image.{BufferedImage}
import javax.swing.{JPanel}
import scala.swing._
import scala.swing.event._

import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._

/*
 * SubScript Exercise
 * 
 * GUI component interaction
 * 
 * A window with a slider with 10 positions for values 1 to 10
 * and buttons for min ("<<"), decrement ("<"), increment (">") and max (">>")
 * These modify the slider value in the usual way.
 * 
 * The slider value is displayed in a label; this label is updated while the user drags the slider.
 * When the value is 1  the min and decrement buttons are disabled
 * When the value is 10 the max and increment buttons are disabled
 * 
 * Alternatively the user may set the value through the number keys: 
 * '1' to '9' set the speed to 1 to 9; key '0' sets the speed to 10.
 */
object SliderDemo extends SliderDemoApp

class SliderDemoApp  extends SimpleSubscriptApplication {
 
  val       minButton = new Button("<<"    ) {enabled       = false; focusable = false; size.width = 20}
  val decrementButton = new Button("<"     ) {enabled       = false; focusable = false; size.width = 20}
  val incrementButton = new Button(">"     ) {enabled       = false; focusable = false; size.width = 20}
  val       maxButton = new Button(">>"    ) {enabled       = false; focusable = false; size.width = 20}
  val      exitButton = new Button("Exit"  ) {enabled       = false; focusable = false}
  val valueLabel      = new Label("value"  ) {preferredSize = new Dimension(65,26)}
  val slider          = new Slider           {min = 1; max = 10}

  val top          = new MainFrame {
    title          = "SliderDemo - Subscript"
    location       = new Point    (100,100)
    preferredSize  = new Dimension(600,400)
    contents       = new FlowPanel(valueLabel, minButton, decrementButton, slider, incrementButton, maxButton)
  }

  // try to listen to the key events....
  top.contents.head.focusable = true
  top.contents.head.requestFocus
  top.listenTo(slider.keys)
  top.listenTo(top.contents.head.keys)
  
  val f = top.peer.getRootPane().getParent().asInstanceOf[javax.swing.JFrame]
  f.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE) // TBD: does not seem to work on MacOS

  def  minValue = slider.min
  def  maxValue = slider.max
  def  value    = slider.value
  def setSliderValue(s: Int) {
    valueLabel.text = "Value: " + s
    slider.value = s  
  }
  def char2Value(ac: Any) = {var c=chr(ac); int2Value(c-'0')}
  def int2Value ( i: Int) = if (i==0) maxValue else minValue+i-1

  // conversion method, unfortunately needed since the SubScript compiler does not handle var types well
  def chr(c:Any) = c.asInstanceOf[Int].toChar
  
  setSliderValue(7)
  
  override def  live = _execute(_live())

  def script.. // utilities
  
    TBD = {*Thread.sleep(34567)*}

    key(   ??c: Char) =  key2(top, ??c)
    numKey(??i: Int)  = var c:Char='0' key(ActualConstrainedParameter(c, (v:Char)=>c=v, (v:Char)=>v.isDigit)) {!i=c-'0'!}    //key(c? if?(c.isDigit)) 
    setValue(s: Int)  = @{gui(there)}: {!setSliderValue(s)!}

  override def script..
    live = TBD
  
  
  def script..
  
    
    
    
    
    
    
    // beware: spoiler below
    
    
    
    
    
    
  
    //////////////////////////////// live     = ...; keyInput + buttonInput + sliderInput
    
    
    
  
  
    // beware: more spoilers below
    
  
  
  
    
    

  ////////////////////////////////////    keyInput = //////////////////////////////////// var i: Int = 0 numKey(ActualOutputParameter(i,(v:Int)=>i=v)) setValue(int2Value(i)) // numKey(i?)
  ////////////////////////////////////               ////////////////////////////////////         
  ////////////////////////////////////   keyInput1 = //////////////////////////////////// times(10) 
  ////////////////////////////////////             + //////////////////////////////////// val c:Any=(pass_up1(here)+'0') key(chr(c)) setValue(char2Value(c)) // TBD: make "here" an implicit parameter
  ////////////////////////////////////               ////////////////////////////////////         
  //////////////////////////////////// buttonInput = //////////////////////////////////// if (value>minValue) (minButton setValue,minValue + decrementButton setValue(value-1))
  ////////////////////////////////////             + //////////////////////////////////// if (value<maxValue) (maxButton setValue,maxValue + incrementButton setValue(value+1))
  ////////////////////////////////////               //////////////////////////////////// 
  //////////////////////////////////// sliderInput = //////////////////////////////////// slider setValue,slider.value


}
