package subscript.exercises

import scala.language.implicitConversions
import java.awt.{Graphics, Image, Color, Point}
import java.awt.image.{BufferedImage}
import javax.swing.{JPanel}
import scala.swing._
import scala.swing.event._
import scala.math._

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
 * A window with a drawing panel that shows TV noise.
 * The user has options to:
 * - start and stop (=freeze) the noise on the panel, using buttons and the Enter key
 * - step, i.e. create a single random pattern on the panel, using a button and the Space key
 * - clear the panel, using a button and the 'c' key
 * - control the speed
 * - exit, which should be confirmed in a dialog box
 * 
 * The noise speed is controlled by a slider, key input and button input,
 * that work as explained in the SliderDemo exercise.
 * 
 * Remarks:
 * The drawing panel is cleared using the method #TVNoisePanel.doClear
 * TVNoise is shown as a sequence of randoming the drawing panel, using the method #TVNoisePanel.doRandomize
 * Between two randomizations the program should wait using the given method #sleep
 * This calls another method named "speed" that should return the value controlled by the slider
 */
object TVNoiseDemo extends TVNoiseDemoApp

class TVNoiseDemoApp  extends SimpleSubscriptApplication {
  import scala.language.implicitConversions
 
  val    tvNoisePanel = new TVNoisePanel
  val     startButton = new Button("Start" ) {enabled       = false; focusable = false}
  val      stopButton = new Button("Stop"  ) {enabled       = false; focusable = false}
  val      stepButton = new Button("Step"  ) {enabled       = false; focusable = false}
  val     clearButton = new Button("Clear" ) {enabled       = false; focusable = false}
  val      exitButton = new Button("Exit"  ) {enabled       = false; focusable = false}
  val       minButton = new Button("<<"    ) {enabled       = false; focusable = false; size.width = 20}
  val decrementButton = new Button("<"     ) {enabled       = false; focusable = false; size.width = 20}
  val incrementButton = new Button(">"     ) {enabled       = false; focusable = false; size.width = 20}
  val       maxButton = new Button(">>"    ) {enabled       = false; focusable = false; size.width = 20}
  val valueLabel      = new Label("value"  ) {preferredSize = new Dimension(65,26)}
  val slider          = new Slider           {min = 1; max = 10}

  val top          = new MainFrame {
    title          = "TVNoiseDemo - Subscript"
    location       = new Point    (100,100)
    preferredSize  = new Dimension(600,400)
    contents       = new BorderPanel {
      add(new BorderPanel {
        add (new FlowPanel(startButton, stopButton,      stepButton,             clearButton,  exitButton), BorderPanel.Position.North)
        add (new FlowPanel(valueLabel ,  minButton, decrementButton, slider, incrementButton,   maxButton), BorderPanel.Position.South) 
      }     , BorderPanel.Position.North)
      add (tvNoisePanel, BorderPanel.Position.Center) 
    }
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
  def  speedValue = slider.value
  def setSliderValue(s: Int) {
    valueLabel.text = "Value: " + s
    slider.value = s  
  }
  def char2Value(ac: Any) = {var c=chr(ac); int2Value(c-'0')}
  def int2Value ( i: Int) = if (i==0) maxValue else minValue+i-1

  // conversion method, unfortunately needed since the SubScript compiler does not handle var types well
  def chr(c:Any) = c.asInstanceOf[Int].toChar

  setSliderValue(7)

    def getSleep_ms = pow(2, 12-speedValue).toInt // logarithmic scale
    
    def sleep = 
      try {
        val sleepPart_ms = 10
        val startTime_ms = System.currentTimeMillis
        while (System.currentTimeMillis - startTime_ms < getSleep_ms) {
          Thread.sleep(sleepPart_ms)
        }
      }
      catch { case e: InterruptedException => /*println("sleep interrupted")*/}

    def sleep_ms(time_ms: Int) = 
      try {
          Thread.sleep(time_ms)
      }
      catch { case e: InterruptedException => println("sleep interrupted")}

  //////////////////////////////////////////////
  // confirm exit dialog
  //////////////////////////////////////////////
  def confirmExit: Boolean = Dialog.showConfirmation(top.contents.head, "Are you sure?", "About to exit")==Dialog.Result.Yes
  
  
  
  override def  live = _execute(_live())

  implicit def script..
  
    key(??c: Char     ) =  key2(top, ??c)
   vkey(??k: Key.Value) = vkey2(top, ??k)

  def script.. // utilities
  
    TBD = {*Thread.sleep(34567)*}

    numKey(??i: Int)       = var c:Char='0' key(ActualConstrainedParameter(c, (v:Char)=>c=v, (v:Char)=>v.isDigit)) {!i=c-'0'!}    //key(c? if?(c.isDigit)) 
    setSpeedValue(s: Int)  = @{gui(there)}: {!setSliderValue(s)!}

  override def script..
    live               = || TBD
  
  def script..
  
    doExit             = exitCommand var r:Boolean=false @{gui(there)}: {r=confirmExit} while (!r)
    doExit1            = exitCommand@{gui(there)}: while (!confirmExit) // dialog run with SwingUtilities.invokeAndWait(): blocks script executions

         clearCommand  = clearButton + 'c'
          stepCommand  = TBD
          exitCommand  = TBD + windowClosing,top
    multiStepStartCmd  = TBD
     multiStepStopCmd  = TBD

   drawingPanelControl = TBD

      do1Step          = @{gui(there)}: tvNoisePanel.doRandomize
      clear            = TBD
      speedControl     = TBD
                    

    
    
    
    
    // beware: spoiler below
    
    
    
    
    
    
  
    //////////////////////////////// live               = || boardControl speedControl doExit
    
    
    
  
  
    // beware: more spoilers below
    
  
  
  
    
    
    
  
  def script..
       //////////////////////////////// 
       ////////////////////////////////       clearCommand  =     ////////////////////////////////     clearButton + 'c'
       ////////////////////////////////        stepCommand  =     ////////////////////////////////      stepButton + ' '
       ////////////////////////////////        exitCommand  =     ////////////////////////////////      exitButton + windowClosing,top
       ////////////////////////////////  multiStepStartCmd  =     ////////////////////////////////     startButton + Key.Enter
       ////////////////////////////////   multiStepStopCmd  =     ////////////////////////////////      stopButton + Key.Enter
       ////////////////////////////////                           //////////////////////////////// 
       ////////////////////////////////  doExit             =     ////////////////////////////////   exitCommand var r:Boolean=false @{gui(there)}: {r=confirmExit} while (!r)
       ////////////////////////////////  doExit1            =     ////////////////////////////////   exitCommand@{gui(there)}: while (!confirmExit) // dialog run with SwingUtilities.invokeAndWait(): blocks script executions
       ////////////////////////////////                           //////////////////////////////// 
       ////////////////////////////////     boardControl    =     //////////////////////////////// ...; (..singleStep) multiStep || clear
       ////////////////////////////////                           //////////////////////////////// 
       ////////////////////////////////    do1Step          =     //////////////////////////////// @{gui(there)}: tvNoisePanel.doRandomize
       ////////////////////////////////                           //////////////////////////////// 
       ////////////////////////////////    clear            =     ////////////////////////////////       clearCommand @{gui(there)}: {!tvNoisePanel.doClear!}
       ////////////////////////////////    singleStep       =     ////////////////////////////////        stepCommand do1Step
       ////////////////////////////////     multiStep       =     //////////////////////////////// multiStepStartCmd; ... do1Step {*sleep*} 
       ////////////////////////////////                     /     //////////////////////////////// multiStepStopCmd
       ////////////////////////////////                           //////////////////////////////// 
       ////////////////////////////////    speedControl     =     //////////////////////////////// ...; speedKeyInput + speedButtonInput + speedSliderInput
       ////////////////////////////////                           //////////////////////////////// 
       ////////////////////////////////    speedKeyInput    =     //////////////////////////////// var i: Int = 0 numKey(ActualOutputParameter(i,(v:Int)=>i=v)) setSpeedValue(int2Value(i)) // numKey(i?)
       ////////////////////////////////                           ////////////////////////////////  
       ////////////////////////////////   speedKeyInput1    =     //////////////////////////////// times(10) 
       ////////////////////////////////                     +     //////////////////////////////// val c:Any=(pass_up1(here)+'0') key(chr(c)) setSpeedValue(char2Value(c)) // TBD: make "here" an implicit parameter
       ////////////////////////////////                           ////////////////////////////////  
       //////////////////////////////// speedButtonInput    =     //////////////////////////////// if (speedValue>minValue) (minButton setSliderValue,minValue + decrementButton setSpeedValue(speedValue-1))
       ////////////////////////////////                     +     //////////////////////////////// if (speedValue<maxValue) (maxButton setSliderValue,maxValue + incrementButton setSpeedValue(speedValue+1))
       ////////////////////////////////                           //////////////////////////////// 
       //////////////////////////////// speedSliderInput    =     //////////////////////////////// slider setSpeedValue,slider.value



}


  /*
   * Panel for displaying TV noise.
   * Largely taken from life.LifeBoard
   * 
   * Methods for external usage:
   *   doRandomize
   *   doClear
   */
  ///////////////////////////////////////////////////////////////////////////////
  class TVNoisePanel(var cellColumns: Int = 200, var cellRows: Int = 150) extends Panel {
    type CellState = Boolean
    val cellSizeX = 3
    val cellSizeY = 3
    var currentX  = -1
    var currentY  = -1
    
    var myForegroundColor = java.awt.Color.black
    var myBackgroundColor = java.awt.Color.white

    var cells        : Array[Array[CellState]] = null
    var bufferedImage: BufferedImage           = null
    var imageGC      : Graphics = null
    
    doClear

    //////////////////////////////////////////////
    // functions for painting and the like
    //////////////////////////////////////////////
    def validate {

        //super.validate
        val newWidth  = size.width
        val newHeight = size.height;
        if (bufferedImage == null) {
            createCells
        } else if (   bufferedImage.getWidth(null)  != newWidth 
                   || bufferedImage.getHeight(null) != newHeight) {
             createCells
        } else {
          renderBufferedImage
        }
        repaint
    }

    def cleanUp {
        if (bufferedImage == null) return
        imageGC.dispose
        bufferedImage.flush
        bufferedImage = null
    }

    /** randomize the canvas with the given density */
    def doRandomize(density: Double = 0.3): Unit = {
        if (bufferedImage==null) createCells
        val random = new java.security.SecureRandom
        cells = Array.fill(cellColumns, cellRows) { random.nextInt(1000) < 1000*density }                       
        createBufferedImage
        renderBufferedImage
        repaint
    }
    def doClear          = doRandomize(0.0)
    def getPreferredSize = new Dimension (cellSizeX*cellColumns, cellSizeY*cellRows);

    def renderBufferedImage {
        if (bufferedImage == null) return
        for (cellX <- 0 until cellColumns;
             cellY <- 0 until cellRows) {
             paintCellInImage (cellX, cellY)
        }
    }
    def update(g: Graphics2D) {
      paint (g);
    }
    
    override def paint (g: Graphics2D) {
        if (bufferedImage == null) {createCells; renderBufferedImage}
        g.drawImage (bufferedImage, 0, 0, null)
    }

    def paintCellInImage(cellX: Int, cellY: Int) {
       imageGC.setColor(if (cells(cellX)(cellY)) myForegroundColor else myBackgroundColor)
       imageGC. fillRect(cellX*cellSizeX, cellY*cellSizeY, cellSizeX, cellSizeY)
    }

    //////////////////////////////////////////////
    // functions implementing the life algorithm
    //////////////////////////////////////////////

    def createCells: Unit = createCells0(size.width, size.height)
    def createCells0 (width: Int, height: Int): Unit = {
        cells         = Array.ofDim[CellState](cellColumns, cellRows)
        createBufferedImage
    }
        
    def createBufferedImage {
        cleanUp
        if (size.width <= 0) return
        bufferedImage = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB);
        imageGC       = bufferedImage.getGraphics
        imageGC.setColor(myBackgroundColor)
        imageGC.fillRect (0,0,bufferedImage.getWidth (null)-1,
                               bufferedImage.getHeight(null)-1)
    }

  } // end of class TVNoisePanel
  ////////////////////////////////////////////////////////////////////
  
