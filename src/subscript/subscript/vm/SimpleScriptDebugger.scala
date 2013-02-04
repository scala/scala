package subscript.vm

/*
 * Simple text based script debugger
 * 
 * Operation mode 1: pass the class to be debugged as parameter from the command line
 * 
 *   execute the main method of SimpleScriptDebuggerObject 
 *   with first argument: the package+class name of the object to be debugged
 *   and later argument: the arguments to be passed to the debugged object
 * 
 * Operation mode 2: pass the debugger as an argument to the subscript.vm._execute method:
 * 
 * 	  val debugger = new SimpleScriptDebugger
 *    _execute(scriptDef, debugger, executor)
 */

object SimpleScriptDebuggerObject extends SimpleScriptDebugger {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) return
    ScriptExecutorFactory.scriptDebugger = this
    val className = args.head
    try {
      val c = Class.forName(className)
      val m = c.getMethod("main", classOf[Array[String]])
      m.invoke(null, args.tail)
    }
    catch {
      case e: ClassNotFoundException => println("Could not find class "+className)
      case other: Throwable => println(other)
    }
  }
}

class SimpleScriptDebugger extends ScriptDebugger {

  def callGraphMessages = scriptExecutor.callGraphMessages
  def rootNode            = scriptExecutor.rootNode
  
  // some tracing stuff
  var nSteps = 0
  var maxSteps = 0 // 0 means unlimited
  val highTraceLevel = 10
  var traceLevel = 2 // 0-no tracing; 1-message handling; 2-message insertion+handling; highTraceLevel-every step a tree 
  def trace(level:Int,as: Any*) = {
    if (traceLevel>=level) {
      as.foreach {a=>print(a.toString)}; 
      println
      //traceMessages
    }
    if (traceLevel >= highTraceLevel) traceTree
    if (maxSteps>0 && nSteps > maxSteps) {println("Exiting after "+nSteps+"steps"); System.exit(0)}
    nSteps += 1
  }
  def traceTree: Unit = {
    var j = 0;
	  def traceTree[T <: TemplateNode](n: CallGraphNodeTrait[T], branches: List[Int], depth: Int): Unit = {
	    for (i<-1 to 30) {
	      print(if(i==depth)"*"else if (branches.contains(i)) "|" else if(j%5==0)"-"else" ")
	    }
	    j+=1
	    println(n)
	    n match {
	      case p:CallGraphParentNodeTrait[_] => 
	        val pcl=p.children.length
	        p.children.foreach{ c =>
	          var bs = if (c.template.indexAsChild<pcl-1) 
	                    depth::branches 
	                    else branches
	          traceTree(c, bs, depth+1)}
	      case _ =>
	    }
	  }
	if (traceLevel >= 1) traceTree(rootNode, Nil, 0)
  }
  def traceMessages: Unit = {
	if (traceLevel >= 1) {
	  println("=== Messages ===")
	  callGraphMessages.foreach(println(_))
	  println("=== End ===")
	}
  }
  
  
  def messageHandled(m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]]): Unit = {
        trace(1,">> ",m)
        m match {
          case AAToBeExecuted(_) =>
            traceTree
            traceMessages
          case _ =>  
        }
  }
  def messageQueued      (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]]                 ) = trace(2, "++ ", m)
  def messageDequeued    (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]]                ) = trace(2, "-- ", m)
  def messageContinuation(m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]], c: Continuation) = trace(2, "** ", c)
  def messageAwaiting: Unit = {traceTree; traceMessages}
}