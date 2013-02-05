package life

import scala.swing._
import scala.swing.event._
import subscript.DSL._
import subscript.swing.SimpleSubscriptApplication

object BasicLifeFrame extends BasicLifeFrameApplication
class BasicLifeFrameApplication extends SimpleSubscriptApplication {
 
  val     board       = new LifeBoard
  val     startButton = new Button("Start" ) {enabled       = false; focusable = false}
  val      stopButton = new Button("Stop"  ) {enabled       = false; focusable = false}
  val      stepButton = new Button("Step"  ) {enabled       = false; focusable = false}
  val randomizeButton = new Button("Random") {enabled       = false; focusable = false}
  val     clearButton = new Button("Clear" ) {enabled       = false; focusable = false}
  val  minSpeedButton = new Button("<<"    ) {enabled       = false; focusable = false; size.width = 20}
  val    slowerButton = new Button("<"     ) {enabled       = false; focusable = false; size.width = 20}
  val    fasterButton = new Button(">"     ) {enabled       = false; focusable = false; size.width = 20}
  val  maxSpeedButton = new Button(">>"    ) {enabled       = false; focusable = false; size.width = 20}
  val      exitButton = new Button("Exit"  ) {enabled       = false; focusable = false}
  val speedLabel      = new Label("speed"  ) {preferredSize = new Dimension(65,26)}
  val speedSlider     = new Slider           {min = 1; max = 10}

  val NO_PATTERN = "None"
  val patternList     = new ListView[String] {listData = NO_PATTERN::ConwayPatterns.allPatterns.map(_._1); selectIndices(0)}
  
  def selectedPatternName = patternList.selection.items.head
  def selectedPattern:Option[String] = if (selectedPatternName==NO_PATTERN) None
                                       else ConwayPatterns.allPatterns.toMap.get(selectedPatternName)
  
  val top          = new MainFrame {
    title          = "Life - Subscript"
    location       = new Point    (100,100)
    preferredSize  = new Dimension(600,400)
    contents       = new BorderPanel {
      add(new BorderPanel {
        add (new FlowPanel(startButton,     stopButton,   stepButton, randomizeButton, clearButton,  exitButton), BorderPanel.Position.North)
        add (new FlowPanel(speedLabel , minSpeedButton, slowerButton, speedSlider, fasterButton, maxSpeedButton), BorderPanel.Position.South) 
      }     , BorderPanel.Position.North)
      add(new BorderPanel { 
        add (new ScrollPane(patternList) {verticalScrollBarPolicy = ScrollPane.BarPolicy.Always; listenTo(keys)}, BorderPanel.Position.West) 
        add (board, BorderPanel.Position.Center) 
      },      BorderPanel.Position.Center)
    }
  }

  // try to listen to the key events....
  top.contents.head.focusable = true
  top.contents.head.requestFocus
  top.listenTo(patternList.keys)
  top.listenTo(speedSlider.keys)
  top.listenTo(top.contents.head.keys)
  
  val f = top.peer.getRootPane().getParent().asInstanceOf[javax.swing.JFrame]
  f.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE) // TBD: does not seem to work on MacOS

  def  minSpeed = speedSlider.min
  def  maxSpeed = speedSlider.max
  def  speed    = speedSlider.value
  def setSpeed(s: Int) {
    speedLabel.text = "Speed: " + s
    speedSlider.value = s  
  }
  def digit2Speed(c: Char) = if (c=='0') maxSpeed else minSpeed+(c-'1')
  
  setSpeed(7)
  
  override def _live = _script(this, 'live) {_threaded0{Thread.sleep(34567)}}
  override def  live = _execute(_live)
}
