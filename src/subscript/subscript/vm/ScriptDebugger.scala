package subscript.vm

trait ScriptDebugger {
  var scriptExecutor: ScriptExecutor = null
  def messageHandled     (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]])
  def messageQueued      (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]])
  def messageDequeued    (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]])
  def messageContinuation(m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]], c: Continuation)
  def messageAwaiting

  def attach(se: ScriptExecutor): Unit = {scriptExecutor = se; se.scriptDebugger = this}
  
}