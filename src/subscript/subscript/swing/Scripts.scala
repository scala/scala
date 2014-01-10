/*
    This file is part of Subscript - an extension of the Scala language 
                                     with constructs from Process Algebra.

    Subscript is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License and the 
    GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    Subscript consists partly of a "virtual machine". This is a library; 
    Subscript applications may distribute this library under the 
    GNU Lesser General Public License, rather than under the 
    GNU General Public License. This way your applications need not 
    be made Open Source software, in case you don't want to.

    Subscript is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You may have received a copy of the GNU General Public License
    and the GNU Lesser General Public License along with Subscript.
    If not, see <http://www.gnu.org/licenses/>
*/

package subscript.swing


import scala.swing._
import scala.swing.event._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._


/*
 * SimpleSubscriptApplication: a SimpleSwingApplication
 * with an abstract "live" script that is started in a new thread
 */
abstract class SimpleSubscriptApplication extends SimpleSwingApplication{
  override def startup(args: Array[String]) {
    super.startup(args)
    new Thread{override def run={live;quit}}.start()
  }
  def _live: N_call => Unit
  def  live: ScriptExecutor
}

/*
 * Scripts for GUI event handling: mouse down, mouse moves, keys, virtual keys, window events
 * Also a method to acquire a CodeExecutor for processing in the swing thread
 */
object Scripts {

  import scala.language.implicitConversions
  
  def gui1[N<:CallGraphNodeTrait](there:N) = {there.adaptExecutor(new SwingCodeExecutorAdapter[CodeExecutorTrait])}
  def gui [N<:Any](there:N)                = {there.asInstanceOf[CallGraphNodeTrait]adaptExecutor(new SwingCodeExecutorAdapter[CodeExecutorTrait])}
  // note that in some cases the type of the "there" parameter is inferred as Any.
  // This is for the time being a workaround:
  // only at the typing phase a refinement call is resoled into either a script call or a method call (=> code fragment)
  // so only then the type of there should be determined.
  // this would require some rework of the compiler: generating code for @...: should only be done in the typer phase
  
//def gui1[N<:CallGraphNodeTrait[_]](implicit n:N) = {n.adaptExecutor(new SwingCodeExecutorAdapter[CodeExecutorTrait])}             

  /*
   * A registry for the ScriptExecutor for which the most recent GUI event had been consumed
   * Note: such executors may invoke one another
   */
  object ScriptReactor {
    var scriptExecutorThatConsumedEvent: ScriptExecutor = null // event.consume not available for button clicks; this consumedEvent item is a workaround
  }
  /*
   * An extension on scala.swing.Reactor that supports event handling scripts in Subscript
   * Allows an event handling script to subscribe and unsubscribe to events
   */
  abstract class ScriptReactor[N<:N_atomic_action] extends Reactor {
    def publisher:Publisher
    var executor: EventHandlingCodeFragmentExecutor[N] = _
    def execute = executeMatching(true)
    def executeMatching(isMatching: Boolean): Unit = executor.executeMatching(isMatching)
    val publisher1 = publisher // needed in subclass since publisher does not seem to be accessible
    private var myEnabled = false
    def enabled = myEnabled
    def enabled_=(b:Boolean) = {myEnabled=b}
    // acknowledgeEventHandled is done when an Event Handling Code Fragment succeeds, performed by the ScriptExecutor
    def acknowledgeEventHandled = {ScriptReactor.scriptExecutorThatConsumedEvent = null} 
    
    val listenedEvent: Event
    var currentEvent : Event = null
    def reaction: PartialFunction[Event,Unit] = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {
      case event if (ScriptReactor.scriptExecutorThatConsumedEvent != executor.scriptExecutor) => {
                 execute
                 if (executor.n.hasSuccess) {
                   ScriptReactor.scriptExecutorThatConsumedEvent = executor.scriptExecutor
                   consumeEvent
                 }
               }
    }
    def consumeEvent = {}
    
    def subscribe(n: N): Unit = {
      executor = new EventHandlingCodeFragmentExecutor(n, n.scriptExecutor)
      n.codeExecutor = executor
      val wasAlreadyEnabled = enabled
      publisher.reactions += reaction;
      if (!wasAlreadyEnabled) {enabled=true}
    }
    def canDisableOnUnsubscribe = true
    def unsubscribe: Unit = {
      publisher.reactions -= reaction
      if (canDisableOnUnsubscribe && !publisher.reactions.isDefinedAt(listenedEvent)) {enabled=false}
    }
  }
  
  /*
   * A Reactor that has a swing Component as a Publisher. 
   * This automatically enables and disables the component
   */
  abstract class EnablingReactor[N<:N_atomic_action](publisher:Publisher with Component, autoEnableComponent: Boolean = true) extends ScriptReactor[N] {
    override def enabled_=(b:Boolean) = {
      super.enabled_=(b)
      if (autoEnableComponent) publisher.enabled = b
    }
  }

  // Note: the following classes have much code in common.
  // This is caused by a dependency on partial functions.
  // In principle this should be cleaned up, but it is not yet clear how that can be done.
  
  // a ComponentReactor for any events
  case class AnyEventReactor[N<:N_atomic_action](comp:Component) extends EnablingReactor[N](comp) {
    def publisher = comp
    val listenedEvent: Event = null
    override def canDisableOnUnsubscribe = false
    private var myReaction: PartialFunction[Event,Unit] = {case e => currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class WindowClosingReactor[N<:N_atomic_action](w:Window) extends ScriptReactor[N] {
    def publisher = w
    val listenedEvent: WindowClosing = null
    override def canDisableOnUnsubscribe = false
    private var myReaction: PartialFunction[Event,Unit] = {case e: WindowClosing => currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class SliderStateChangedReactor[N<:N_atomic_action](s:Slider) extends ScriptReactor[N] {
    def publisher = s
    val listenedEvent: ValueChanged = null
    override def canDisableOnUnsubscribe = false
    private var myReaction: PartialFunction[Event,Unit] = {case e: ValueChanged => currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  
  /*
   * A Reactor that has a swing Component as a Publisher. 
   */
  abstract class ComponentReactor[N<:N_atomic_action](comp:Component) extends ScriptReactor[N] {
    def publisher = comp
    override def canDisableOnUnsubscribe = false
  }
  
  // TBD: compact these classes someventhandlingow...
  case class MouseClickedReactor[N<:N_atomic_action](comp:Component, forClicksCount: Int = 0) extends ComponentReactor[N](comp) {
    val listenedEvent: MouseClicked = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MouseClicked =>
      if (forClicksCount==0 
      ||  forClicksCount==e.clicks) {currentEvent=e; execute; currentEvent=null}}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class MousePressedReactor[N<:N_atomic_action](comp:Component) extends ComponentReactor[N](comp) {
    val listenedEvent: MousePressed = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MousePressed =>
      currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class MouseReleasedReactor[N<:N_atomic_action](comp:Component) extends ComponentReactor[N](comp) {
    val listenedEvent: MouseReleased = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MouseReleased =>
      currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class MouseMovedReactor[N<:N_atomic_action](comp:Component) extends ComponentReactor[N](comp) {
    val listenedEvent: MouseMoved = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MouseMoved =>
      currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class MouseDraggedReactor[N<:N_atomic_action](comp:Component) extends ComponentReactor[N](comp) {
    val listenedEvent: MouseDragged = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MouseDragged =>
      currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  
  /*
   * A EnablingReactor for clicked events on a button
   * TBD: a way to consume clicked events on the button
   */
  case class ClickedReactor[N<:N_atomic_action](button:AbstractButton) extends EnablingReactor[N](button) {
    val wasFocusable = button.focusable
    override def enabled_=(b:Boolean) = {
      super.enabled_=(b)
      button.focusable = wasFocusable
    }
    def publisher = button
    val listenedEvent: Event = ButtonClicked(button)
    override def consumeEvent = {
      listenedEvent match {
        case ie: InputEvent => ie.consume // unfortunately, this is not applicable
        case _ => // no consume event option seems to be available
    } }
  }
 
  /*
   * A Reactor for key typed events
   */
  case class KeyTypedReactor[N<:N_atomic_action](publisher:Publisher, keyCode: FormalConstrainedParameter[Char]) extends ScriptReactor[N] {
    val listenedEvent = null
    override def reaction = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {
      case KeyTyped(comp, char, keyModifiers, keyLocationValue) => 
        if (char < 256) {
	      if (keyCode.matches(char)) {
	        keyCode.value = char
	        executeMatching(true)
	      }
        }
    }
    override def unsubscribe: Unit = {
      publisher1.reactions -= reaction
    }
  }
  
  /*
   * A Reactor for virtual key press events
   */
  case class VKeyTypedReactor[N<:N_atomic_action](publisher:Publisher, keyValue: FormalConstrainedParameter[Key.Value]) extends ScriptReactor[N] {
    val listenedEvent = null
    override def reaction = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {
      case KeyPressed(comp, keyPressedValue, keyModifiers, keyLocationValue) => 
        if (keyValue.matches(keyPressedValue)) {
          keyValue.value = keyPressedValue
          executeMatching(true)
        }
    }
    override def unsubscribe: Unit = {
      publisher1.reactions -= reaction
    }
  }

  case class KeyTypedEventReactor[N<:N_atomic_action](publisher:Publisher, keyTypedEvent: FormalConstrainedParameter[KeyTyped]) extends ScriptReactor[N] {
    val listenedEvent = null
    override def reaction = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {
      case kt@KeyTyped(comp, char, keyModifiers, keyLocationValue) =>
        if (keyTypedEvent.matches(kt)) {
          keyTypedEvent.value = kt
          executeMatching(true)
        }
    }
    override def unsubscribe: Unit = {publisher1.reactions -= reaction}
  }
  case class KeyTypedEventsReactor[N<:N_atomic_action](publisher:Publisher) extends ScriptReactor[N] {
    val listenedEvent = null
    override def reaction = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {case e: KeyTyped =>
      currentEvent=e; execute; currentEvent=null
    }
    override def unsubscribe: Unit = {publisher1.reactions -= reaction}
  }

 /*
  * The following subscript code has manually been compiled into Scala; see below
    The redirections to the swing thread using "@gui:" are needed 
    because enabling and disabling the button etc must there be done
  */
 implicit def script ..  // TBD: handle tabs in scanner so that line position becomes reasonable
   stateChange(slider: Slider)                   = event(SliderStateChangedReactor[N_code_eventhandling](slider))
   clicked(button:Button)                        = event(           ClickedReactor[N_code_eventhandling](button))

 def script ..   // TBD: uncomment /*@gui:*/ and make it compile
  event[E <: Event] (reactor:ScriptReactor[N_code_eventhandling], ?e: E) =  /*@gui:*/ @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: {. e = reactor.currentEvent.asInstanceOf[E] .}
  event (reactor:ScriptReactor[N_code_eventhandling]) =  /*@gui:*/ @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: {.     .}
  event_loop(reactor:ScriptReactor[N_code_eventhandling_loop], task: MouseEvent=>Unit)   =  /*@gui:*/ @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: 
                                                                                            {... task.apply(reactor.currentEvent.asInstanceOf[MouseEvent]) ...}
  event_loop_KTE(reactor:ScriptReactor[N_code_eventhandling_loop], task: KeyTyped=>Unit)   =  /*@gui:*/ @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: 
                                                                                            {... task.apply(reactor.currentEvent.asInstanceOf[KeyTyped]) ...}
  // TBD: MouseEvent should become type parameter, as in the following (which does not compile)
  //event_loop[E<:Event](reactor:Reactor[N_code_eventhandling_loop], task: E=>Unit)   =  /*@gui:*/ @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: 
  //                                                                                                {... task.apply(reactor.currentEvent.asInstanceOf[E]) ...}
       anyEvent(comp: Component)                           = event(          AnyEventReactor[N_code_eventhandling](comp))                                                    
    windowClosing(window: Window)                          = event(     WindowClosingReactor[N_code_eventhandling](window))
// mouseClicks   (comp: Component, task: MouseEvent=>Unit) = event_loop( MouseClickedReactor[N_code_eventhandling_loop](comp), task)
   mousePresses  (comp: Component, task: MouseEvent=>Unit) = event_loop( MousePressedReactor[N_code_eventhandling_loop](comp), task)
   mouseDraggings(comp: Component, task: MouseEvent=>Unit) = event_loop( MouseDraggedReactor[N_code_eventhandling_loop](comp), task)
   mouseMoves    (comp: Component, task: MouseEvent=>Unit) = event_loop(   MouseMovedReactor[N_code_eventhandling_loop](comp), task)

   mouseSingleClick (comp: Component, ?p : java.awt.Point) = mouseClicks(1, comp, ?p) // TBD: "p?"
   mouseDoubleClick (comp: Component, ?p : java.awt.Point) = mouseClicks(2, comp, ?p)
   mouseTripleClick (comp: Component, ?p : java.awt.Point) = mouseClicks(3, comp, ?p)
   mouseClicks(n:Int,comp: Component, ?p : java.awt.Point) = var mce: MouseClicked=null 
                                                             event(MouseClickedReactor[N_code_eventhandling](comp, n), ActualOutputParameter(mce, (v:MouseClicked)=>mce=v) ) // TBD ...
                                                             {! p=mce.point !}
   mouseMove        (comp: Component, ?p : java.awt.Point) = var mme: MouseMoved=null 
                                                             event(   MouseMovedReactor[N_code_eventhandling](comp), ActualOutputParameter(mme, (v:MouseMoved)=>mme=v) ) // TBD: "mme?" instead of "ActualOutputParameter(...)"
                                                             {! p=mme.point !}
   mousePressed     (comp: Component, ?p : java.awt.Point) = var mpe: MousePressed=null 
                                                             event(   MousePressedReactor[N_code_eventhandling](comp), ActualOutputParameter(mpe, (v:MousePressed)=>mpe=v) ) // TBD: ...
                                                             {! p=mpe.point !}
   mouseReleased    (comp: Component, ?p : java.awt.Point) = var mre: MouseReleased=null 
                                                             event(   MouseReleasedReactor[N_code_eventhandling](comp), ActualOutputParameter(mre, (v:MouseReleased)=>mre=v) ) // TBD: ...
                                                             {! p=mre.point !}

/* these 4 scripts should become:
   mouseClicks(n:Int,comp: Component, ?p : java.awt.Point) = var mce: MouseClicked =null; event( MouseClickedReactor[N_code_eventhandling](comp, n), ?mce); {! p=mce.point !}
   mouseMove        (comp: Component, ?p : java.awt.Point) = var mme: MouseMoved   =null; event(   MouseMovedReactor[N_code_eventhandling](comp   ), ?mme); {! p=mme.point !}
   mousePressed     (comp: Component, ?p : java.awt.Point) = var mpe: MousePressed =null; event( MousePressedReactor[N_code_eventhandling](comp   ), ?mpe); {! p=mpe.point !}
   mouseReleased    (comp: Component, ?p : java.awt.Point) = var mre: MouseReleased=null; event(MouseReleasedReactor[N_code_eventhandling](comp   ), ?mre); {! p=mre.point !}

for now:

error: missing parameter type for expanded function ((x$4) => _mce.at(here).value = x$4)
mouseClicks(n:Int,comp: Component, ?p : java.awt.Point) = var mce: MouseClicked =null; event( MouseClickedReactor[N_code_eventhandling](comp, n), ?mce); {! p=mce.point !}
                                                                                                                                                   ^
*/

     guard(comp: Component, test: () => Boolean)           = if (test()) .. else ... anyEvent(comp)

     key2(publisher: Publisher, ??keyCode : Char     )     = event(         KeyTypedReactor[N_code_eventhandling](publisher, _keyCode ))
    vkey2(publisher: Publisher, ??keyValue: Key.Value)     = event(        VKeyTypedReactor[N_code_eventhandling](publisher, _keyValue))
    
     keyEvent2 (publisher: Publisher, ??keyTypedEvent : KeyTyped)  = event(         KeyTypedEventReactor [N_code_eventhandling](publisher, _keyTypedEvent))
     keyEvents2(publisher: Publisher, task: KeyTyped=>Unit)        = event_loop_KTE(KeyTypedEventsReactor[N_code_eventhandling_loop](publisher), task)
}