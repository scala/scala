package subscript.exercises

import scala.swing._
import scala.swing.Swing._
import scala.swing.event._
 
import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._

object Interact extends InteractApp

/*
 * SubScript Exercise
 * 
 * GUI component interaction, taken from Rosetta Code: http://rosettacode.org/wiki/GUI_component_interaction
 * 
 * (...) a form with three components to the user: 
 *   A numeric input field ("Value") and two buttons ("increment" and "random").
 * The field is initialized to zero. 
 * The user may manually enter a new value into the field, 
 * or increment its value with the "increment" button. 
 * Entering a non-numeric value should be either impossible, or issue an error message.
 * Pressing the "random" button presents a confirmation dialog, 
 * and resets the field's value to a random value if the answer is "Yes".
 * 
 * To filter non-numeric key input the top window should listen to key events targeted to the text field.
 * Each event that has a non-numeric char value should be consumed.
 */
class InteractApp extends SimpleSubscriptApplication {
  import scala.language.implicitConversions

  // arrange buttons in a grid with 1 row, 2 columns
  // arrange text field and button panel in a grid with 2 row, 1 column

  val     incButton = new Button    { text = "Increment"; enabled = false}
  val    randButton = new Button    { text = "Random"   ; enabled = false}
  val   numberField = new TextField { text = "0" } // start at 0
  val   buttonPanel = new GridPanel(1, 2) {contents ++= List(  incButton,  randButton)}
  val contentsPanel = new GridPanel(2, 1) {contents ++= List(numberField, buttonPanel)}

  val top = new MainFrame {
    title    = "Rosetta Code >>> Task: component interaction | Language: Scala"
    contents = contentsPanel
    listenTo(numberField.keys)
  }
  
  /*
   * Run a confirmation dialog for the Randomize action
   */
  def confirmRandom: Boolean = Dialog.showConfirmation(top.contents.head, "Are you sure?", "About to randomize")==Dialog.Result.Yes

  /*
   * Perform the increment action
   */
  def doIncrement = {if (numberField.text.isEmpty()) numberField.text = "0"
                                  // we use BigInt to avoid long overflow/number format exception
                                  val biNumber = new BigInt(new java.math.BigInteger(numberField.text))
                                  numberField.text = "" + (biNumber+1)}
  
  /*
   * Perform the Randomize action
   */
  def doRandomize = {numberField.text = (Long.MaxValue*Math.random).toLong.toString}
  
  override def  live = _execute(_live())
  
  implicit def script..
    /*
     * Helper scripts for key events
     */
    keyEvent (??keyTypedEvent: KeyTyped) = keyEvent2( top, ??keyTypedEvent)  // keyEvent2(top, keyTypedEvent?)
    keyEvents(task: KeyTyped=>Unit)      = keyEvents2(top, task)  //keyEvents2(top, keyTypedEvent)
    
    /*
     * Consume a key event when the given condition is met.
     * Note that this script starts listening to a KeyTyped event; 
     * when such an event arrives and the condition is met,
     * the event is consumed and associated atomic action happens,
     * and that ends the listening to KeyTyped events
     * 
     * It is possible to activate this script thereafter again, 
     * but this takes some time, and meanwhile a key event may slip through.
     * There are two solutions to this:
     * 
     * 1. Make sure the atomic action does not happen, 
     *    e.g., using the following as second parameter in the call to keyEvent:
     *          (v:KeyTyped) => {if(cond(v.char)) v.consume;  false}
     *    
     * 2. Use script consumeKeys instead
     */
    consumeKey(cond: Char=>Boolean)      = var k: KeyTyped=null 
                                        // keyEvent(? k if(cond(k)) {k.consume; true} else false) TBD...FTTB:
                                           keyEvent(ActualConstrainedParameter(k, (v:KeyTyped)=>k=v, (v:KeyTyped) => {if(cond(v.char)) {v.consume; true} else false}))

    /*
     * Consume all key events for which the given condition is met.
     * This script calls keyEvents, which in turn calls Scripts.keyEvents2,
     * and the latter applies a looping event handling code fragment.
     * 
     * This means that the script does its task for all KeyTyped events; 
     * listening does not stop, so no key event may slip through.
     * 
     * Compare script consumeKey that applies a normal event handling code fragment
     * so key events may slip through
     */
    consumeKeys(cond: Char=>Boolean)     = keyEvents((v:KeyTyped) => if(cond(v.char)) v.consume)

  def script..
    
    /*
     * Create the live script that consumes non digit keys, 
     * and that allows the user to increment and randomize the number 
     * by clicking the respective buttons
     * 
     * Try the key filtering in two ways:
     * 1. with the script consumeKey in a loop; this should occasionally go wrong
     * 2. with a single call to script consumeKeys
     */
    live                 = ...

    
    
    
    
    
    
    
    // beware: spoiler below
    
    
    
    
    
    
  
    ////////////////////////////////live                 = consumeNonDigitKeys || increments || randomizations
    
    
    
    
    
    
    // beware: more spoilers below
    
    
    
    
    
    
    
    
    ////////////////////////////////consumeNonDigitKeys  = ////////////////////////////////////////////////////////////consumeKeys((c:Char) => !c.isDigit)
    ////////////////////////////////consumeNonDigitKeys1 = ////////////////////////////////////////////////////////////consumeKey( (c:Char) => !c.isDigit)...
    ////////////////////////////////increments           = //////////////////////////////////////////////////////////// incButton @{gui(there)}: {doIncrement} ...
    ////////////////////////////////randomizations       = ////////////////////////////////////////////////////////////randButton @{gui(there)}: if (confirmRandom) @{gui(there)}: {doRandomize}  ...
}