package life

import scala.math._
import scala.swing._
import scala.swing.event._
import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._

object LifeFrame extends LifeFrameApplication
class LifeFrameApplication extends BasicLifeFrameApplication {

    //////////////////////////////////////////////
    // speed control
    //////////////////////////////////////////////
    
    def getSleep_ms = pow(2, 12-speed).toInt // logarithmic scale
    
    def sleep = 
      try {
        val sleepPart_ms = 10
        val startTime_ms = System.currentTimeMillis
        while (System.currentTimeMillis - startTime_ms < getSleep_ms) {
          Thread.sleep(sleepPart_ms)
        }
      }
      catch { case e: InterruptedException => println("sleep interrupted")}

    //////////////////////////////////////////////
    // confirm exit dialog
    //////////////////////////////////////////////
    def confirmExit: Boolean = Dialog.showConfirmation(top.contents.head, "Are you sure?", "About to exit")==Dialog.Result.Yes
     
    board.listenTo(board.mouse.clicks)
    board.listenTo(board.mouse.moves)

    //////////////////////////////////////////////
    // handle MouseDown events
    //////////////////////////////////////////////
     def doMouseDown (e: MouseEvent): Unit = {
       selectedPattern match {
          case None => board.mouseDownToggle(e)
          case Some(s) => val ec = Coord(e.point.x/board.cellSizeX, e.point.y/board.cellSizeY)
                          for (pc <- ConwayPatterns.moveTo(s,ec)) {board.setCellValue(pc.x,pc.y,true)}
       }
     }
     def doMouseDrag (e: MouseEvent): Unit = board.mouseDragToggle(e)
    
     def chr(c:Any) = c.asInstanceOf[Int].toChar
     
  implicit def script..
  
    key(c??: Char     ) =  key2(top, c)  //key(top, c??) // TBD in subscript-scalac parser: recognize the ??
   vkey(k??: Key.Value) = vkey2(top, k) //vkey(top, k??)

def script..
	 randomizeCommand  = randomizeButton + 'r'
	     clearCommand  =     clearButton + 'c'
	      stepCommand  =      stepButton + ' '
	      exitCommand  =      exitButton + windowClosing(top)
	multiStepStartCmd  =     startButton + Key.Enter
	 multiStepStopCmd  =      stopButton + Key.Enter
	
	doExit             =   exitCommand var r:Boolean=false @gui: {r=confirmExit} while (!r)
	
       boardControl    = ...; (..singleStep) multiStep || noise || clear || randomize

      do1Step          = {*board.calculateGeneration*} @gui: {!board.validate!}
      
      noise            = 'n'; ... @gui: board.doRandomize {*sleep*}
      randomize        =   randomizeCommand @gui: {!board.doRandomize()!}
      clear            =       clearCommand @gui: {!board.doClear!}
      singleStep       =        stepCommand do1Step
       multiStep       = multiStepStartCmd; ... do1Step {*sleep*} / multiStepStopCmd

      speedControl     = ...; speedKeyInput + speedButtonInput + speedSliderInput
                    
    setSpeed(s: Int)   = @gui: {!setSpeedValue(s)!}

      speedKeyInput    = times(10) 
                       + val c:Any=(pass_up1(here)+'0') key(chr(c)) setSpeed(digit2Speed(chr(c))) // TBD: make here an implicit parameter
                              
   speedButtonInput = if (speed>minSpeed) speedDecButton
                    + if (speed<maxSpeed) speedIncButton
    
     speedDecButton = minSpeedButton setSpeed(minSpeed)
                    +   slowerButton setSpeed(speed-1)
     
     speedIncButton = maxSpeedButton setSpeed(maxSpeed)
                    +   fasterButton setSpeed(speed+1)
     
   speedSliderInput = speedSlider setSpeed(speedSlider.value)

      mouseInput    = mousePressInput & mouseDragInput

    mousePressInput = mousePresses  (board, (e: MouseEvent) => doMouseDown(e))
    mouseDragInput  = mouseDraggings(board, (e: MouseEvent) => doMouseDrag(e))  

override def script..
    live            = boardControl 
                   || mouseInput 
                   || speedControl  
                   || doExit
                   
  /* the subscript code above had manually been compiled into Scala:
   * 
  def _vkey(_k:FormalConstrainedParameter[Key.Value]) = _script(this, 'vkey, _k~??'k) {subscript.swing.Scripts._vkey(top, _k~??)}
  def  _key(_c:FormalConstrainedParameter[Char     ]) = _script(this,  'key, _c~??'c) {subscript.swing.Scripts._key (top, _c~??)}
               
  override def _live     = _script(this,  'live            ) {_par_or2(_boardControl, _mouseInput, _speedControl, _exit)}
  def  _randomizeCommand = _script(this,  'randomizeCommand) {_alt(_clicked(randomizeButton), _key('r'))} 
  def      _clearCommand = _script(this,      'clearCommand) {_alt(_clicked(    clearButton), _key('c'))}
  def       _stepCommand = _script(this,       'stepCommand) {_alt(_clicked(     stepButton), _key(' '))}
  def       _exitCommand = _script(this,       'exitCommand) {_alt(_clicked(     exitButton), _windowClosing(top))}
  def _multiStepStartCmd = _script(this, 'multiStepStartCmd) {_alt(_clicked(    startButton), _vkey(Key.Enter))}
  def  _multiStepStopCmd = _script(this,  'multiStepStopCmd) {_alt(_clicked(     stopButton), _vkey(Key.Enter))}
  
  def   _exit            = {val _r = _declare[Boolean]('r)
                           _script(this, 'exit) {_seq(_var(_r, (here:N_localvar[_]) => false), 
                                                      _exitCommand,
                                                      _at{gui0} (_normal{here => _r.at(here).value = confirmExit}),
                                                      _while{here=> {! _r.at(here).value}})}
                            }

  def _boardControl      = _script(this, 'boardControl    ) {_seq(_loop, _par_or2(_disrupt(_noise, _seq(_seq(_optionalBreak_loop, _singleStep), _multiStep)), _randomize, _clear))} 
  def _do1Step           = _script(this, 'do1Step         ) {_seq(_threaded0{board.calculateGeneration}, _at{gui0}(_tiny0{board.validate}))} 

  def _noise             = _script(this, 'noise           ) {_seq( _key('n'), _seq(_loop, _at{gui0}(_normal0{board.doRandomize()}), _threaded0{sleep}))} 
  def _randomize         = _script(this, 'randomize       ) {_seq( _randomizeCommand, _at{gui0}(_tiny0{board.doRandomize()}))} 
  def _clear             = _script(this, 'clear           ) {_seq(     _clearCommand, _at{gui0}(_tiny0{board.doClear}))} 
  def _singleStep        = _script(this, 'singleStep      ) {_seq(      _stepCommand, _do1Step)} 
  def _multiStep         = _script(this, 'multiStep       ) {_seq(_multiStepStartCmd, _disrupt(_seq(_loop, _do1Step, _threaded0{sleep}), _multiStepStopCmd))} 
      
  def _setSpeed(_s:FormalInputParameter[Int])  = _script(this, 'setSpeed, _s~'s) {_at{gui0}(_tiny0{setSpeed(_s.value)})} 
  
  def _speedControl      = _script(this, 'speedControl    ) {_seq(_loop, _alt(_speedKeyInput, _speedButtonInput, _speedSliderInput))} 

  def _speedKeyInput     = {val _c = _declare[Char]('c)
                           _script(this, 'speedKeyInput   ) {_alt(_times(10), _seq(_val(_c, implicit here => (pass_up1+'0').toChar), 
                                                                                   _call{here=>_key(_c.at(here).value)(here)}, 
                                                                                   _call{here=>_setSpeed(digit2Speed(_c.at(here).value))(here)} ))}
                           }
  def _speedButtonInput  = _script(this, 'speedButtonInput) {_alt(_if0{speed>minSpeed}(_buttonSpeedDec), _if0{speed<maxSpeed}(_buttonSpeedInc))} 

  def _buttonSpeedDec    = _script(this, 'buttonSpeedDec  ) {_alt(_seq(_clicked(minSpeedButton), _setSpeed(minSpeed)), _seq(_clicked(slowerButton), _setSpeed(speed-1)))} 
  def _buttonSpeedInc    = _script(this, 'buttonSpeedInc  ) {_alt(_seq(_clicked(maxSpeedButton), _setSpeed(maxSpeed)), _seq(_clicked(fasterButton), _setSpeed(speed+1)))} 
  
  def _speedSliderInput  = _script(this, 'speedSliderInput) {_seq(_stateChange(speedSlider), _setSpeed(speedSlider.value))} 
     
  def _mouseInput        = _script(this, 'mouseInput      ) {_par(_mousePressInput, _mouseDragInput)} 

  def _mousePressInput   = _script(this, 'mousePressInput ) {_mousePresses  (board, (e: MouseEvent) => doMouseDown(e))} 
  def _mouseDragInput    = _script(this, 'mouseDragInput  ) {_mouseDraggings(board, (e: MouseEvent) => doMouseDrag(e))} 

  // bridge method   
  override def live = _execute(_live)
*/

}


