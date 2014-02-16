package subscript.vm

import akka.actor.Actor
import subscript.DSL._scriptType
import scala.collection.mutable.ListBuffer

trait SubScriptActor extends Actor {

  // Live script
  def _live(): N_call => Unit
  
  // Termination script
  private def script terminate = {*Terminator.block*}
  private object Terminator {
    def block = synchronized(wait())
    def release = synchronized(notify())
  }
  
  // Handlers to handle messages
  private val callHandlers = ListBuffer[Actor.Receive]()
  
  
  // Callbacks
  
  override def aroundPreStart() {
    def script lifecycle = live || terminate  // TBD: (live || terminate) ; killActor
    SubScriptActor.executeScript(_lifecycle())
    super.aroundPreStart()
  } 
  
  override def aroundReceive(receive: Actor.Receive, msg: Any) {
    val messageWasHandled = callHandlers.synchronized {
      // Wait for handlers
      if (callHandlers.isEmpty) callHandlers.wait()
      
      // If at least for one time the message was handled, the result of synchronized block will be true
      // Else - false
      callHandlers.foldLeft(false) {(flag, handler) =>
        if (handler.isDefinedAt(msg)) {
          handler(msg)
          handler.synchronized {handler.notify()}  // Kick the receiver, so that blocked registerReceiver() can proceed
          callHandlers -= handler
          true
        } else flag
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
  
 
  def registerReceiver(receiver: PartialFunction[Any, Unit]) {    
    var receiverFired = false
    lazy val spyedReceiver: PartialFunction[Any, Unit] = receiver andThen {_ =>
      spyedReceiver.synchronized {receiverFired = true}
    }
    
    callHandlers.synchronized {
      spyedReceiver +=: callHandlers
      callHandlers.notify()  // Now aroundReceive can proceed in case it waited on the empty collection
    }
    
    // registerReceiver is the only place where VM will interact with the actor
    // before it's "<<>>" fragment will be executed. It's natural to block here, so that
    // the "<<>>" fragment will terminate only when the desired message is actually handled
    spyedReceiver.synchronized {if (!receiverFired) spyedReceiver.wait()}
  }
  
  
  // Make `receive` final - it doesn't needed anyway
  final def receive: Actor.Receive = {case _ =>}
}

object SubScriptActor {
  
  private var vm: CommonScriptExecutor = null
  private var parallelOp: CallGraphParentNodeTrait = null
  
  private object Stopper {
    def block   = synchronized(wait())
    def release = synchronized(notify())
  }
  private def script parallelScript = {*Stopper.block*} & (+)
  
  def executeScript(script: _scriptType) = synchronized {
    // If VM is null, spawn it and call normal code
    if (vm == null) prepareVm()
    val template = extractTemplate(script)
    vm.activateFrom(parallelOp, template)
  }
  
  def prepareVm() {
    vm = ScriptExecutorFactory.createScriptExecutor(true);
    _parallelScript()(vm.anchorNode)
    new Thread {override def run = {vm.run}}.start()
    
    // While the dynamic call graph is not initialized to the point that
    // parallel operator is available
    // TBD add synchronization
    while (vm.anchorNode.children.isEmpty || vm.anchorNode.children.head.asInstanceOf[CallGraphParentNodeTrait].children.isEmpty)
      Thread.sleep(10)
    
    parallelOp =
      vm.anchorNode.children.head.asInstanceOf[CallGraphParentNodeTrait].children.head.asInstanceOf[CallGraphParentNodeTrait]
  }
  
  def extractTemplate(script: _scriptType) = {
    val extractor = N_call(T_call(null))
    script(extractor)
    
    // TBD: parallelize code in the extracted template
    extractor.t_callee
  }
  
  def releaseVm() = Stopper.release
}