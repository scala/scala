package subscript.vm

import akka.actor.Actor
import subscript.DSL._scriptType
import scala.collection.mutable.ListBuffer

trait SubScriptActor extends Actor {

  private object Terminator {
    def block = synchronized(wait())
    def release = synchronized(notify())
  }
  
  private val callHandlers = ListBuffer[PartialFunction[Any, Unit]]()

  
  
  // Scripts
  def _live(): N_call => Unit
  private def script terminate = {*Terminator.block*}
  private def script die       = {if (context ne null) context stop self}
  
  
  
  // Callbacks
  override def aroundPreStart() {
    def script lifecycle = (live || terminate) ; die
    SubScriptActor.executeScript(_lifecycle())
    super.aroundPreStart()
  } 
  
  override def aroundReceive(receive: Actor.Receive, msg: Any) {    
    synchronized {
      sendSynchronizationMessage(this)
      wait()
    }
    
    var messageWasHandled = false
    callHandlers.synchronized {
      for (h <- callHandlers if h isDefinedAt msg) {
        h(msg)
        messageWasHandled = true
      }
    }
    
    // If a message was handled, Akka will try to match it against a function that can handle any message
    // otherwise it will try to match the message against function that can handle virtually nothing
    // (except LocalObject, which is certainly local and can't be available from the outside)
    case object LocalObject
    super.aroundReceive(if (messageWasHandled) {case _: Any =>} else {case LocalObject =>}, msg)
  }
  
  override def aroundPostStop() {
    Terminator.release
    super.aroundPostStop()
  }
  
  final def receive: Actor.Receive = {case _ =>}
 
  
  
  // SubScript actor convenience methods
  def initActor(node: N_code_eventhandling, _handler: PartialFunction[Any, Unit]) {
    node.codeExecutor = EventHandlingCodeFragmentExecutor(node, node.scriptExecutor)
    val handler = _handler andThen {_ => node.codeExecutor.executeAA}
    synchronized {callHandlers += handler}
    node.onDeactivate {
      synchronized {callHandlers -= handler}
    }
  }
    
  def sendSynchronizationMessage(lock: AnyRef) {
    val vm = SubScriptActor.vm
    vm insert SynchronizationMessage(vm.rootNode, lock)    
  }

}


object SubScriptActor {
  
  private lazy val vm: CommonScriptExecutor = {
    val _vm = ScriptExecutorFactory.createScriptExecutor(true);
    
    _parallelScript()(_vm.anchorNode)
    _vm addHandler synchMsgHandler
    
    new Thread {override def run = {_vm.run}}.start()
    while (parallelOp == null) wait()
    
    _vm
  }
  private var parallelOp: CallGraphParentNodeTrait = null
  
  private object Stopper {
    def block   = synchronized(wait())
    def release = synchronized(notify())
  }
  private def script parallelScript = {*Stopper.block*} & {captureParallelOp(here)}
  private def captureParallelOp(here: CallGraphTreeNode) = synchronized {
    parallelOp = here.parent.asInstanceOf[CallGraphParentNodeTrait]
    notify()
  }
  
  val synchMsgHandler: PartialFunction[CallGraphMessage[_ <: CallGraphNodeTrait], Unit] = {
    case SynchronizationMessage(_, lock) => lock.synchronized(lock.notify())
  }
    
  def executeScript(script: _scriptType) = synchronized {
    val template = extractTemplate(script)
    vm.invokeFromET { vm.activateFrom(parallelOp, template) }
  }
  
  
  def extractTemplate(script: _scriptType) = {
    val extractor = N_call(T_call(null))
    script(extractor)
    extractor.t_callee
  }
  
  def releaseVm() = Stopper.release
  
}

case class SynchronizationMessage(node: CallGraphNodeTrait, lock: AnyRef) extends CallGraphMessage[CallGraphNodeTrait] {
  priority = -1
}