package subscript.vm

trait ScriptDebugger {
  var scriptExecutor: ScriptExecutor = null
  def messageHandled     (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait])
  def messageQueued      (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait])
  def messageDequeued    (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait])
  def messageContinuation(m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait], c: Continuation)
  def messageAwaiting

  def attach(se: ScriptExecutor): Unit = {scriptExecutor = se; se.scriptDebugger = this}
  
}