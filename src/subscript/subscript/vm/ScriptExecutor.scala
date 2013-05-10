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

package subscript.vm
import scala.collection.mutable._

/*
 * Factory for script executors. Produces a CommonScriptExecutor
 */
object ScriptExecutorFactory {
  var scriptDebuggerQueue = new scala.collection.mutable.Queue[ScriptDebugger]
  def addScriptDebugger(sd: ScriptDebugger) = {
    //println("addScriptDebugger: "+sd.getClass.getCanonicalName)
    scriptDebuggerQueue += sd
  }
  def createScriptExecutor(allowDebugger: Boolean) = {
    val se = new CommonScriptExecutor
    if (allowDebugger && !scriptDebuggerQueue.isEmpty) {
      val h = scriptDebuggerQueue.head
      //println("createScriptExecutor: "+se+ " Debugger: "+h.getClass.getCanonicalName)
      scriptDebuggerQueue = scriptDebuggerQueue.tail
      h.attach(se)
    }
    //else println("createScriptExecutor: "+se+" allowDebugger: "+allowDebugger+" scriptDebuggerQueue.isEmpty: "+scriptDebuggerQueue.isEmpty)
    se
  }
}

/*
 * Trait for Script executors
 */
trait ScriptExecutor {
  
  def rootNode: N_launch_anchor
  
  val anchorNode: N_call
  def hasSuccess: Boolean
  def run: ScriptExecutor
  def insert(sga: CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]])
  
  /*
   * Call graph message ordering
   */
  val CallGraphMessageOrdering = 
	  new Ordering[CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]]] {
    def compare(x: CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]], 
                y: CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]]): Int = {
	        val p = x.priority - y.priority
	        if (p != 0) {p} // highest priority first
	        else if (x.isInstanceOf[Continuation         ]) {  x.node.index - y.node.index}  // newest nodes first
	        else if (x.isInstanceOf[AAToBeReexecuted[_,_]]) {- x.     index + y.     index}  // oldest messages first, so that retries are FIFO
	        else                                            {- x.node.index + y.node.index}  // oldest nodes first
        }
	}
  
  /*
   * methods for enqueueing and dequeueing callGraphMessages
   *
   * TBD: the callGraphMessages queue should become synchronized for 
   *      AAExecutionFinished, AAToBeReexecuted and Deactivation messages, as these may be inserted
   *      asynchronously by a CodeExecutor (for threaded code, gui thread code and event handling code)
   *      Also, it would become faster if it has internally separate queues for each priority level
   * 
   */
  // TBD: AAToBeReexecuted messages should be FIFO
  var callGraphMessageCount = 0
  val callGraphMessages = new PriorityQueue[CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]]]()(CallGraphMessageOrdering)
  def queueCallGraphMessage(m: CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]]) = {
    callGraphMessages.synchronized {
      callGraphMessages += m
      callGraphMessageCount += 1
      if (callGraphMessageCount != callGraphMessages.length) {
        println("queue: "+callGraphMessageCount+" != " + callGraphMessages.length)
      }
    }
  }
  def dequeueCallGraphMessage: CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]] = {
    callGraphMessages.synchronized {
      callGraphMessageCount -= 1
      val result = callGraphMessages.dequeue
      if (callGraphMessageCount != callGraphMessages.length) {
        println("dequeue: "+callGraphMessageCount+" != " + callGraphMessages.length)
      }
      result
    }
  }
  
  /*
   * methods supporting debuggers
   */
  var scriptDebugger: ScriptDebugger = null;
  def messageHandled     (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]]                 ) = if (scriptDebugger!=null) scriptDebugger.messageHandled (m)
  def messageQueued      (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]]                 ) = if (scriptDebugger!=null) scriptDebugger.messageQueued  (m)
  def messageDequeued    (m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]]                 ) = if (scriptDebugger!=null) scriptDebugger.messageDequeued(m)
  def messageContinuation(m: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode]], c: Continuation) = if (scriptDebugger!=null) scriptDebugger.messageContinuation(m, c)
  def messageAwaiting                                                                                                                 = if (scriptDebugger!=null) scriptDebugger.messageAwaiting
  
  /*
   * Message index and node index generators
   */
  var nMessages = 0
  def nextMessageID() = {nMessages+=1; nMessages}

  var nNodes = 0
  def nextNodeIndex() = {nNodes = nNodes+1; nNodes}
  
  /*
   * Simple tracing and error tracking
   */
  def trace(s: String) = {}// println(s)
  def error(s: String) {throw new Exception(s)}
}

/*
 * Common script executor
 * 
 * TBD
 *   clean up
 *   operators such as % ! -  ~
 *   communication
 *   networks and pipes
 *   match & for operands
 *   exception handling 
 */

class CommonScriptExecutor extends ScriptExecutor {
  
 
  // send out a success when in an And-like context
  def doNeutral(n: CallGraphNode[_<:TemplateNode]) =
    if (n.getLogicalKind_n_ary_op_ancestor!=LogicalKind.Or) {
         insert(Success(n))
    }
  // .. ... for and while operate on the closest ancestor node that has an n_ary operator
  def setIteration_n_ary_op_ancestor(n: CallGraphNode[_<:TemplateNode]) = {
    val a = n.n_ary_op_ancestor
    if (a!=null) a.isIteration = true
  }

  // connect a parent and a child node in the graph
  def connect(parentNode: CallGraphParentNodeTrait[_<:TemplateNode], childNode: CallGraphTreeNode[_<:TemplateNode]) {
    childNode.parent = parentNode
    childNode.scriptExecutor = parentNode.scriptExecutor
    parentNode.children.append(childNode)
    if (parentNode.isInstanceOf[CallGraphTreeNode_n_ary]) {
      val p = parentNode.asInstanceOf[CallGraphTreeNode_n_ary]
      p.lastActivatedChild = childNode
    }
  }
  // disconnect a child node from its parent
  def disconnect(childNode: CallGraphNodeTrait[_<:TemplateNode]) {
    if (childNode.isInstanceOf[CallGraphTreeNode[_<:TemplateNode]]) {
      val ctn = childNode.asInstanceOf[CallGraphTreeNode[_<:TemplateNode]]
      val parentNode = ctn.parent
      if (parentNode==null) return;
      parentNode.children -= ctn
    }
  }
 

  // insert a message in the queue
  def insert(m: CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]]) = {
    m.index = nextMessageID()
    messageQueued(m)
    queueCallGraphMessage(m)
    m match {
      case maa@AAToBeExecuted  (n: CallGraphNodeWithCodeTrait[_,_]) => n.asInstanceOf[N_atomic_action[_]].msgAAToBeExecuted = maa
      case maa@AAToBeReexecuted(n: CallGraphNodeWithCodeTrait[_,_]) => n.asInstanceOf[N_atomic_action[_]].msgAAToBeExecuted = maa
      case _ =>
    }
  }
  // remove a message from the queue
  def remove(m: CallGraphMessage[_ <: CallGraphNodeTrait[_<:TemplateNode]]) = {
    messageDequeued(m)
    //scriptGraphMessages -= m  is not allowed...FTTB we will ignore this message, by checking the canceled flag in the executor
    m match {
      case maa@AAToBeExecuted  (n: CallGraphNodeWithCodeTrait[_,_]) => n.asInstanceOf[N_atomic_action[_]].msgAAToBeExecuted = null
      case maa@AAToBeReexecuted(n: CallGraphNodeTrait          [_]) => n.asInstanceOf[N_atomic_action[_]].msgAAToBeExecuted = null
      case _ =>
    }
  }
  def insertDeactivation(n:CallGraphNodeTrait[_ <: TemplateNode],c:CallGraphNodeTrait[_ <: TemplateNode]) = insert(Deactivation(n, c, false))
  // insert a continuation message
  def insertContinuation(message: CallGraphMessage[_<:CallGraphNodeTrait[_<:TemplateNode]], child: CallGraphTreeNode[_<:TemplateNode] = null): Unit = {
    val n = message.node.asInstanceOf[CallGraphTreeNode_n_ary]
    var c = n.continuation 
    
    // Continuations are merged with already existing ones
    // TBD: make separate priorities of continuations...
    // e.g. a continuation for AAActivated should not be merged (probably) with one for AAStarted
    if (c==null) {
      c = new Continuation(n)
    }
    if (c.childNode==null) // should be improved
    {
      c.childNode = 
        if (child!=null) child
        else message match {
          case Break(an,c,m) => c
          case Success(an,c) => c
          case _ => message.node
        }
    }
    message match {
      case a@ Activation  (node: CallGraphNodeTrait[_]) => c.activation = a
      case a@Deactivation (node: CallGraphNodeTrait[_],
                          child: CallGraphNodeTrait[_], excluded: Boolean) => c.deactivations ::= a
      case a@Success      (node: CallGraphNodeTrait[_],
                          child: CallGraphNodeTrait[_])  => c.success = a
      case a@Break        (node: CallGraphNodeTrait[_], 
                          child: CallGraphNodeTrait[_], 
                 activationMode: ActivationMode.ActivationModeType)  => c.break = a
      case a@AAActivated  (node: CallGraphNodeTrait[_], 
                          child: CallGraphNodeTrait[_]) =>  c.aaActivated = a
      case a@CAActivated  (node: CallGraphNodeTrait[_], 
                          child: CallGraphNodeTrait[_]) =>  c.caActivated = a
      case a@AAStarted    (node: CallGraphNodeTrait[_], 
                          child: CallGraphNodeTrait[_]) =>  c.aaStarteds ::= a
      case a@AAEnded      (node: CallGraphNodeTrait[_], 
                          child: CallGraphNodeTrait[_]) =>  c.aaEndeds ::= a
    }
    if (n.continuation==null) {
        n.continuation = c
       insert (c)
    }
    else {
      messageContinuation(message, c)
    }
  }
  // insert a continuation message for a unary operator
  def insertContinuation1(message: CallGraphMessage[_<:CallGraphNodeTrait[_<:TemplateNode]]): Unit = {
    val n = message.node.asInstanceOf[N_1_ary_op]
    var c = n.continuation
    if (c==null) {
      c = new Continuation1(n)
      n.continuation = c
      queueCallGraphMessage(c)
    }
    queueCallGraphMessage(Continuation1(n))
  }
  
  def hasSuccess     = rootNode.hasSuccess
  
  var aaStartedCount = 0; // count for started atomic actions; TBD: use for determining success
  
  /*
   * A root node is at the top of the script call graph.
   * Directly below that is an anchor node, i.e. a script call node; 
   * below it the callee script will pend, i.e. the script that has been called from Scala.
   */
  val anchorTemplate = new T_call(null)
  val rootTemplate   = new T_1_ary("**", anchorTemplate) {override def root=this; override def owner=CommonScriptExecutor.this}
  val rootNode       = new N_launch_anchor(rootTemplate)
  val anchorNode     = new N_call(anchorTemplate)
  anchorTemplate.parent = rootTemplate
  
  rootNode.scriptExecutor = this 
  connect(parentNode = rootNode, childNode = anchorNode)
  
  /*
   * activate a new node from the given parent node using the given template.
   * The optional pass is relevant in case the parent is an n_ary operator
   */
  def activateFrom(parent: CallGraphParentNodeTrait[_<:TemplateNode], template: TemplateNode, pass: Option[Int] = None): CallGraphTreeNode[_<:TemplateNode] = {
    val n = createNode(template)
    n.pass = pass.getOrElse(if(parent.isInstanceOf[N_n_ary_op]) 0 else parent.pass)
    connect(parentNode = parent, childNode = n)
    if (n.isInstanceOf[N_script]) {
      val ns = n.asInstanceOf[N_script]
      val pc = ns.parent.asInstanceOf[N_call]
    }
    insert(Activation(n))
    n
  }

  /*
   * Create a node according to the given template
   */
  def createNode(template: TemplateNode): CallGraphTreeNode[_<:TemplateNode] = {
   val result =
    template match {
      case t @ T_0_ary     ("."                  ) => N_optional_break(t)
      case t @ T_0_ary     (".."                 ) => N_optional_break_loop(t)
      case t @ T_0_ary     ("..."                ) => N_loop          (t)
      case t @ T_0_ary     ("break"              ) => N_break         (t)
      case t @ T_0_ary     ("(-)"                ) => N_delta         (t)
      case t @ T_0_ary     ("(+)"                ) => N_epsilon       (t)
      case t @ T_0_ary     ("(+-)"               ) => N_nu            (t)
      case t @ T_call      (              _      ) => N_call          (t)
      case t @ T_0_ary_name("private"   , name   ) => N_privatevar    (t.asInstanceOf[T_0_ary_name[N_privatevar   ]])
      case t @ T_0_ary_local_valueCode(kind,lv:LocalVariable[_],_) => N_localvar    (t,isLoop=kind=="val..."||kind=="var...")
      case t @ T_0_ary_code("{}"        , _      ) => N_code_normal   (t.asInstanceOf[T_0_ary_code[N_code_normal  ]])
      case t @ T_0_ary_code("{??}"      , _      ) => N_code_unsure   (t.asInstanceOf[T_0_ary_code[N_code_unsure  ]])
      case t @ T_0_ary_code("{!!}"      , _      ) => N_code_tiny     (t.asInstanceOf[T_0_ary_code[N_code_tiny    ]])
      case t @ T_0_ary_code("{**}"      , _      ) => N_code_threaded (t.asInstanceOf[T_0_ary_code[N_code_threaded]])
      case t @ T_0_ary_code("{..}"      , _      ) => N_code_eh       (t.asInstanceOf[T_0_ary_code[N_code_eh      ]])
      case t @ T_0_ary_code("{......}"  , _      ) => N_code_eh_loop  (t.asInstanceOf[T_0_ary_code[N_code_eh_loop ]])
      case t @ T_0_ary_test("while"     , _      ) => N_while         (t.asInstanceOf[T_0_ary_test[N_while        ]])
      case t @ T_1_ary     ("*"         , _      ) => N_launch        (t)
      case t @ T_1_ary     ("**"        , _      ) => N_launch_anchor (t)
      case t @ T_1_ary     (kind: String, _      ) => N_1_ary_op      (t)
      case t @ T_annotation(              _, _   ) => N_annotation    (t)
      case t @ T_1_ary_test("if"        , _, _   ) => N_if            (t.asInstanceOf[T_1_ary_test[N_if]])
      case t @ T_2_ary_test("if_else"   , _, _, _) => N_if_else       (t.asInstanceOf[T_2_ary_test[N_if_else]])
      case t @ T_2_ary     ("?"         , _, _   ) => N_inline_if     (t)
      case t @ T_3_ary     ("?:"        , _, _, _) => N_inline_if_else(t)
      case t @ T_n_ary(kind: String, children@ _*) => N_n_ary_op      (t, T_n_ary.isLeftMerge(kind))
      case t @ T_script(_, kind: String, name: Symbol, 
                        child0: TemplateNode     ) => N_script        (t.asInstanceOf[T_script    ])
      case _ => null 
    }
    result.codeExecutor = defaultCodeFragmentExecutorFor(result)
    result
  }
  /*
   * The default code executor for a given node.
   */
  def defaultCodeFragmentExecutorFor(node: CallGraphNodeTrait[_<:TemplateNode]): CodeExecutorTrait = {
    node match {
      case n@N_code_normal  (_) => new   NormalCodeFragmentExecutor(n, this)
      case n@N_code_unsure  (_) => new   UnsureCodeFragmentExecutor(n, this)
      case n@N_code_threaded(_) => new ThreadedCodeFragmentExecutor(n, this)
      case _                    => new          TinyCodeExecutor(node, this)
    }
  }
  /*
   * Methods for executing code
   */
  def executeCode_localvar      (n: N_localvar[_]   ) = /*if (n.template.code!=null)*/ executeCode(n, ()=>n.template.code.apply.apply(n)) /*else 0 what to use as default?*/
  def executeCode_call          (n: N_call          ) = {
    var v = executeTemplateCode[N_call,T_call, N_call=>Unit](n)
    v(n)
  }
  def executeCode_annotation[CN<:CallGraphNodeTrait[CT],CT<:TemplateChildNode](n: N_annotation[CN,CT]) = executeTemplateCode[N_annotation[CN,CT], T_annotation[CN,CT],Unit](n)
  
  def executeTemplateCode[N<:CallGraphNodeWithCodeTrait[T,R],T<:TemplateChildNodeWithCode[N,R],R](n: N): R = {
    executeCode(n, 
        ()=>n.template.code.apply.apply(n))
  }
  def executeCode[R](n: CallGraphNodeTrait[_], code: =>()=>R): R = {
    n.codeExecutor.doCodeExecution(code)
  }
  def executeCodeIfDefined(n: CallGraphNodeTrait[_], code: =>()=>Unit): Unit = {
    if (code!=null) executeCode(n, code)
  }
  /*
   * Handle a deactivation message. 
   * If the receiving node is a n_ary operator and there is a sending child node,
   * then postpone further processing by inserting a continuation message.
   * TBD: add a case for a 1_ary operator
   * 
   * Insert deactivation messages for all parent nodes
   * Execute code for deactivation, if defined
   * Unlink the node from the call graph
   */
  def handleDeactivation(message: Deactivation): Unit = {
       message.node match {
           case n@N_n_ary_op (_: T_n_ary, _  )  => if(message.child!=null) {
                                                     if (message.child.hasSuccess) {
                                                        n.childThatEndedInSuccess_index(message.child.index)
                                                     }
                                                     else {
                                                        n.aChildEndedInFailure = true
                                                     }
                                                     insertContinuation(message); 
                                                     return}
           case _ => 
      }
      message.node.forEachParent(p => insertDeactivation(p,message.node))
      executeCodeIfDefined(message.node, message.node.onDeactivate)
      executeCodeIfDefined(message.node, message.node.onDeactivateOrSuspend)
      disconnect(childNode = message.node)
  }

  /*
   * Handle an activation message.
   * This involves:
   * 
   * execute activation code, if defined
   * execute specific code, e.g. for if, while, script call
   * insert an activation message to create a child node
   */
  def handleActivation(message: Activation): Unit = {
      executeCodeIfDefined(message.node, message.node.onActivate)
      executeCodeIfDefined(message.node, message.node.onActivateOrResume)
      message.node match {
           //case n@N_root            (t: T_1_ary     ) => activateFrom(n, t.child0)
           case n@N_code_tiny       (t: T_0_ary_code[_])  => n.hasSuccess = true; executeTemplateCode[N_code_tiny, T_0_ary_code[N_code_tiny], Unit](n); if (n.hasSuccess) doNeutral(n); insertDeactivation(n,null)
           case n@N_localvar        (t: T_0_ary_local_valueCode[_], isLoop)  => if (isLoop) setIteration_n_ary_op_ancestor(n); 
            val v = executeCode_localvar(n);n.n_ary_op_ancestor.initLocalVariable(t.localVariable.name, n.pass, v); doNeutral(n); insertDeactivation(n,null)
           case n@N_privatevar      (t: T_0_ary_name[_]) => n.n_ary_op_ancestor.initLocalVariable(t.name, n.pass, n.getLocalVariableHolder(t.name).value)
           case n@N_code_normal     (_: T_0_ary_code[_]) => insert(AAActivated(n,null)); insert(AAToBeExecuted(n))
           case n@N_code_unsure     (_: T_0_ary_code[_]) => insert(AAActivated(n,null)); insert(AAToBeExecuted(n))
           case n@N_code_threaded   (_: T_0_ary_code[_]) => insert(AAActivated(n,null)); insert(AAToBeExecuted(n))

           case n@( N_code_eh       (_: T_0_ary_code[_]) 
                  | N_code_eh_loop  (_: T_0_ary_code[_])) => // ehNodesAwaitingExecution.append(n) not used; could be handy for debugging
              
           case n@N_break           (t: T_0_ary        ) => doNeutral(n); insert(Break(n, null, ActivationMode.Inactive)); insertDeactivation(n,null)
           case n@N_optional_break  (t: T_0_ary        ) => doNeutral(n); insert(Break(n, null, ActivationMode.Optional)); insertDeactivation(n,null)
           case n@N_optional_break_loop
                                    (t: T_0_ary        ) => setIteration_n_ary_op_ancestor(n); doNeutral(n); insert(Break(n, null, ActivationMode.Optional)); insertDeactivation(n,null)
           case n@N_loop            (t: T_0_ary        ) => setIteration_n_ary_op_ancestor(n); doNeutral(n); insertDeactivation(n,null)
           case n@N_delta           (t: T_0_ary        ) =>                     insertDeactivation(n,null)
           case n@N_epsilon         (t: T_0_ary        ) => insert(Success(n)); insertDeactivation(n,null)
           case n@N_nu              (t: T_0_ary        ) => doNeutral(n);       insertDeactivation(n,null)
           case n@N_while           (t: T_0_ary_test[_]) => setIteration_n_ary_op_ancestor(n); 
                                                            n.hasSuccess = executeTemplateCode[N_while, T_0_ary_test[N_while], Boolean](n)
                                                            doNeutral(n)
                                                            if (!n.hasSuccess) {
                                                               insert(Break(n, null, ActivationMode.Inactive))
                                                            }
                                                            insertDeactivation(n,null)
                                                            
           case n@N_launch          (t: T_1_ary        ) => activateFrom(CallGraphNode.getLowestLaunchAnchorAncestor(n), t.child0, Some(0)); insertDeactivation(n,null)
           case n@N_launch_anchor   (t: T_1_ary        ) => activateFrom(n, t.child0, Some(0))
           case n@N_1_ary_op        (t: T_1_ary        ) => activateFrom(n, t.child0); insertContinuation1(message)
           case n@N_annotation      (t: T_annotation[_,_]) => activateFrom(n, t.child0); executeCode_annotation(n)
           case n@N_if              (t: T_1_ary_test[_]) => if (executeTemplateCode[N_if     , T_1_ary_test[N_if     ], Boolean](n)) activateFrom(n, t.child0) else {doNeutral(n); insertDeactivation(n,null)}
           case n@N_if_else         (t: T_2_ary_test[_]) => if (executeTemplateCode[N_if_else, T_2_ary_test[N_if_else], Boolean](n)) activateFrom(n, t.child0) 
                                                                     else  activateFrom(n, t.child1)
           case n@N_inline_if       (t: T_2_ary        ) => activateFrom(n, t.child0)
           case n@N_inline_if_else  (t: T_3_ary        ) => activateFrom(n, t.child0)
           case n@N_n_ary_op        (t: T_n_ary, 
                                         isLeftMerge   ) => val cn = activateFrom(n, t.children.head); if (!isLeftMerge) insertContinuation(message, cn)
           case n@N_call            (t: T_call         ) => executeCode_call(n);
                                                            if (n.t_callee!=null) {activateFrom(n, n.t_callee)}
                                                            else {
                                                              insert(CAActivated   (n,null))
                                                              insert(CAActivatedTBD(n))
                                                            }
           case n@N_script          (t: T_script       ) => activateFrom(n, t.child0)
      }      
  }
  
  /*
   * Handle a success message
   * 
   * for n_ary and 1_ary nodes if the message comes from a child node:
   *   postpone further processing by inserting a continuation message 
   *   
   * for a script call node: transfer parameters
   * 
   * set the node's hadSuccess flag
   * execute "onSuccess" code, if defined
   * insert success messages for each parent node
   */
  def handleSuccess(message: Success): Unit = {
          message.node match {
               case n@  N_annotation    (_) => {} // onSuccess?
               case n@  N_inline_if     (t: T_2_ary        )  => if (message.child.template==t.child0) {
                                                                              activateFrom(n, t.child1)
                                                                              return
                                                                       }
               case n@  N_inline_if_else(t: T_3_ary        )  => if (message.child.template==t.child0) {
                                                                              activateFrom(n, t.child1)
                                                                              return
                                                                       }
               case n@  N_1_ary_op      (t: T_1_ary        )  => if(message.child!=null) {
                                                                   insertContinuation1(message) 
                                                                   return
                                                                 }
               case n@  N_n_ary_op      (_: T_n_ary, _      ) => if(message.child!=null) {
                                                                   insertContinuation(message) 
                                                                   return
                                                                 }
               case n@  N_call          (_: T_call          ) => if (!n.allActualParametersMatch) {return}
                                                                 n.transferParameters
               case _ => {}
          }
         message.node.hasSuccess = true
         executeCodeIfDefined(message.node, message.node.onSuccess)
         message.node.forEachParent(p => insert(Success(p, message.node)))
  }
  /*
   * Handle an AAActivated message: activated atomic actions 
   * for n_ary and 1_ary nodes if the message comes from a child node:
   *   postpone further processing by inserting a continuation message 
   *   
   * insert AAActivated messages for each parent node
   */
  def handleAAActivated(message: AAActivated): Unit = {
          message.node match {
               case n@  N_1_ary_op      (t: T_1_ary        )  => if(message.child!=null) {
                                                                   insertContinuation1(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case n@  N_n_ary_op      (_: T_n_ary, _     )  => if(message.child!=null) {
                                                                   insertContinuation(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case _ => 
          }
          message.node.forEachParent(p => insert(AAActivated(p, message.node)))
  }
  /*
   * Handle an CAActivated message: activated communications 
   * This may be of interest for a "+" operator higher up in the graph: 
   *   it may have to proceed with activating a next operand, 
   *   in case it had been "paused" by a optional break operand (. or ..)
   *
   * for n_ary and 1_ary nodes if the message comes from a child node:
   *   postpone further processing by inserting a continuation message 
   *   
   * insert CAActivated messages for each parent node
   */
  def handleCAActivated(message: CAActivated): Unit = {
          message.node match {
               case n@  N_1_ary_op      (t: T_1_ary        )  => if(message.child!=null) {
                                                                   insertContinuation1(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case n@  N_n_ary_op      (_: T_n_ary, _     )  => if(message.child!=null) {
                                                                   insertContinuation(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case _ => 
          }
          message.node.forEachParent(p => insert(CAActivated(p, message.node)))
  }
  
  /*
   * Communication handling features: still in the think&try phase
   * Not ready for testing
   */
  def handleCAActivatedTBD(message: CAActivatedTBD): Unit = {
    if (CommunicationMatchingMessage.activatedCommunicatorCalls.isEmpty) {
      insert(CommunicationMatchingMessage)
    }
    CommunicationMatchingMessage.activatedCommunicatorCalls += message.node
  }
  // TBD: process all fresh CA nodes to activate prospective communications
 def handleCommunicationMatchingMessage = {
    var startingCommunicationsOpenForMorePartners: List[N_communication] = Nil
    for (acc <- CommunicationMatchingMessage.activatedCommunicatorCalls) {
      // check out the associated communication relations:
      // if one of these may be activated with help of pending CA partners, then do so
      // else make this CA call pending as well
      
      var i = 0
      while (i < acc.communicator.roles.length && acc.children.isEmpty) {
        val cr = acc.communicator.roles(i)
        i += 1
        tryCommunication(acc, cr)
      }
      if (acc.children.isEmpty) {
        acc.communicator.instances += acc // TBD: remove again when acc is deactivated, using stopPending
      }
    }
    CommunicationMatchingMessage.activatedCommunicatorCalls.clear()
  }
        
	// a communication may become active when
	// all mandatory positions for partners at the same instance may be filled
	// partners should have compatible parameters, be active in parallel, and obey network topology
  def tryCommunication(freshCall: N_call, freshCallRole: CommunicatorRole): Boolean = {
    val communication = freshCallRole.communication
    
    def tryCommunicationWithPartners(partners: List[N_call]): Boolean = {
        def canCommunicateWithPartners(n: N_call): Boolean = {
          var i = 0
          while (i < partners.length) {
            // TBD: check for parallel locations
            val p = partners(i)
            val r = communication.communicatorRoles(i)
            // TBD: check parameter compatibilities
            
            // TBD: check network topology
          }
          return true
        }
	    var i = partners.length // TBD: make it a List[List[N_call]]
        if (i == communication.communicatorRoles.length) {  
           val nc = N_communication(communication.template)
           // set nc.partners vv and make it activate
           nc.communication = communication
           nc.parents ++: partners
           for (p<-partners) {p.children+=nc}
           //executeCode_call(nc);
           //activateFrom(nc, n.t_callee)}
           return true
        }
        else {
	      val role = communication.communicatorRoles(i)
	      val nodes = if (role==freshCallRole) List(freshCall)
	                else  role.communicator.instances
          var j = 0
          while (j < nodes.length) {
	        val node = role.communicator.instances(j)
            j += 1
            if (canCommunicateWithPartners(node)) {
              if (tryCommunicationWithPartners(node::partners)) {
                return true;
              }
            }
          }
	      return false
	    }
    }
    // TBD: first try comms that may still grow (having multipicities other than One)
    return tryCommunicationWithPartners(Nil)
  }
	          

  /*
   * Handle an AAStarted message
   *
   * Resets the node's hadSuccess flag
   * Increments the busyActions count
   * 
   * If the node is an n_ary or 1_ary operator: insert a continuation message 
   * If the node is a suspending operator: decide on what children to suspend
   * If the node is an exclusive opeator: decide on what children to exclude
   * 
   * Insert the exclude messages and suspend messages
   * For each parent node insert an AAStarted message
   * 
   * TBD: ensure that an n-ary node gets only 1 AAStarted msg 
   * after an AA started in a communication reachable from multiple child nodes (*)
   */
  def handleAAStarted(message: AAStarted): Unit = {
    message.node.hasSuccess   = false
    message.node.numberOfBusyActions += 1; 
    message.node match {
       case n@N_1_ary_op(t: T_1_ary)    => insertContinuation1(message) //don't return; just put the continuations in place
       case n@N_n_ary_op(_: T_n_ary, _) => insertContinuation (message) //don't return; just put the continuations in place, mainly used for left merge operators
	                                                                   
	        // decide on exclusions and suspensions; deciding on exclusions must be done before activating next operands, of course
	        var nodesToBeSuspended: Buffer[CallGraphNodeTrait[_ <:TemplateNode]] = null
	        var nodesToBeExcluded : Buffer[CallGraphNodeTrait[_ <:TemplateNode]] = null
			if (T_n_ary.isSuspending(n.template)) {
		      val s = message.child
		      if (s.aaStartedCount==1) {
		        n.template.kind match {
		          case "#" | "#%#"   => nodesToBeSuspended = n.children - s 
		          case "#%"          => nodesToBeExcluded  = n.children.filter(_.index < s.index) 
		                                nodesToBeSuspended = n.children.filter(_.index > s.index)
		          case "#/" | "#/#/" => nodesToBeSuspended = n.children.filter(_.index < s.index) 
		        }
		      }
			}
			else {
	          n.template.kind match {
		          case ";" | "|;" 
		           | "||;" | "|;|" 
		           | "+"   | "|+"  => nodesToBeExcluded = n.children.filter(_.index != message.child.index)
		                         // after (*), do: nodesToBeExcluded = n.children -- message.aaStarteds.map( (as:AAStarted) => as.child)  
		                  
		          case "/" | "|/" 
		             | "|/|"       => nodesToBeExcluded = n.children.filter(_.index < message.child.index)
		                              // after (*), do: 
		                              // val minIndex = message.child.index
		                              // nodesToBeExcluded = n.children -- message.aaStarteds.map( (as:AAStarted) => as.child)
		          case _ =>
	          }
	        }
		    // do exclusions and suspensions
		    if (nodesToBeExcluded !=null) nodesToBeExcluded .foreach((n) => insert(Exclude(message.node, n)))
		    if (nodesToBeSuspended!=null) nodesToBeSuspended.foreach((n) => insert(Suspend(n)))
       case _ =>	    
	  }
      // message.child may be null now
      message.node.forEachParent(p => insert(AAStarted(p, message.node)))
  }
  /*
   * Handle an AAEnded message
   *
   * Resets the node's hadSuccess flag
   * Decrements the busyActions count
   * 
   * If the node is an n_ary or 1_ary operator: insert a continuation message 
   *
   * For each parent node insert an AAStarted message
   * 
   * TBD: ensure that an n-ary node gets only 1 AAStarted msg 
   * after an AA started in a communication reachable from multiple child nodes (*)
   */
  def handleAAEnded(message: AAEnded): Unit = {
    message.node.numberOfBusyActions -= 1; 
    message.node.hasSuccess = false
	  message.node match {
               case n@  N_1_ary_op      (t: T_1_ary        )  => if(message.child!=null) {
                                                                   insertContinuation1(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case n@  N_n_ary_op      (_: T_n_ary, _     )  => if(message.child!=null) {
                                                                   insertContinuation(message)
                                                                   //don't return; just put the continuations in place
                                                                  }
              case _ => 
	  }
      message.node.forEachParent(p => insert(AAEnded(p, message.node)))
  }
  
  /*
   * Handle a break message (break  .   ..)
   * 
   * if the node is an n_ary operator:
   *   if the node is not already inactive, set its activation mode to the one specified by the break message
   *   insert a continuation message
   * else insert break messages for each parent node
   */
  def handleBreak(message: Break): Unit = {
      message.node match {
        case nn:CallGraphTreeNode_n_ary =>
	        if (nn.activationMode!=ActivationMode.Inactive) {
	            nn.activationMode = message.activationMode
	        }
	        insertContinuation(message)
        case _ => message.node.forEachParent(p => insert(Break(p, message.node, message.activationMode)))
      }
  }
  
  /*
   * Handle an exclude message
   * 
   * Set the node's isExcluded flag
   * Interrupt asynchronously running code for the node, if any
   * 
   * If the node is a communication partner: make it stop pending (TBD)
   * If the node is an atomic action: remove AAToBeExecuted message, if any 
   *         (TBD: also remove AAToBeReExecuted message, if any?)
   *         inset a deactivation message
   * insert exclude messages for each child node
   */
  def handleExclude(message: Exclude): Unit = { // TBD: remove messages for the node; interrupt execution
    val n = message.node
    n.isExcluded = true
    
    if (       n.isInstanceOf[CallGraphNodeWithCodeTrait[_,_]]) {
      val nc = n.asInstanceOf[CallGraphNodeWithCodeTrait[_,_]]
      if (nc.codeExecutor != null) {
        nc.codeExecutor.interruptAA
      }
    }
    message.node match {
      case cc: N_call => cc.stopPending
      case aa: N_atomic_action[_] =>
        aa.codeExecutor.cancelAA
        if (aa.msgAAToBeExecuted != null) {
          remove(message) // does not really remove from the queue; will have to check the canceled flag of the codeExecutor...
          aa.msgAAToBeExecuted = null
        }
        // TBD: also for caNodes!!
        insert(Deactivation(aa, null, excluded=true))
      case _ =>
    }
    if (      n.isInstanceOf[CallGraphTreeParentNode[_<:TemplateNode]]) {
      val p = n.asInstanceOf[CallGraphTreeParentNode[_<:TemplateNode]]
      p.forEachChild(c => insert(Exclude(n,c)))
      return
    }
  }
	
  /*
   * Handle a continuation message for an unary node
   */
  def handleContinuation1(message: Continuation1): Unit = {
    val n = message.node.asInstanceOf[N_1_ary_op]
    n.continuation = null
    // TBD
  }
  
  /*
   * Handle an AAToBeExecuted message
   * 
   * perform the codeExecutor's executeAA method
   * 
   * Note: the message may have been canceled instead of removed from the queue (was easier to implement),
   * so for the time being check the canceled flag
   */
  def handleAAToBeExecuted[T<:TemplateChildNodeWithCode[_,R],R](message: AAToBeExecuted[T,R]) {
    val e = message.node.codeExecutor
    if (!e.canceled)  // temporary fix, since the message queue does not yet allow for removals
         e.executeAA
  }
  /*
   * Handle an AAToBeReexecuted message
   * 
   * insert an AAToBeExecuted message
   * 
   * Note: the message may have been canceled instead of removed from the queue (was easier to implement),
   * so for the time being check the canceled flag
   */
  def handleAAToBeReexecuted[T<:TemplateChildNodeWithCode[_,R],R](message: AAToBeReexecuted[T,R]) {
    val e = message.node.codeExecutor
    if (!e.canceled) // temporary fix, since the message queue does not yet allow for removals
       insert(AAToBeExecuted(message.node)) // this way, failed {??} code ends up at the back of the queue
  }
  /*
   * Handle an AAExecutionFinished message
   * 
   * perform the codeExecutor's afterExecuteAA method,
   * which may insert success and deactivation messages in turn
   * 
   * Note:
   * A node's code executor has just finished execution. This may have been done asynchronously.
   * It has inserted an AAExecutionFinished, so that this will be handled synchronously in the main script executor loop.
   *
   */
  def handleAAExecutionFinished[T<:TemplateChildNodeWithCode[_,R],R](message: AAExecutionFinished[T,R]) {
     message.node.codeExecutor.afterExecuteAA
  }
  
  /*
   * handleContinuation:
   * 
   * The most complicated method of the Script Executor: determine what an N-ary operator will do
   * after it has received a set of messages. Potential outcomes include:
   * - activate next operand and/or have success
   * - suspend, resume, exclude
   * - deactivate
   * - nothing
   *   
   * The decision is based on three aspects:
   * - the kind of operator
   * - the state of the node
   * - the received messages
  */ 
  def handleContinuation(message: Continuation): Unit = {
    val n = message.node.asInstanceOf[CallGraphTreeNode_n_ary]
    n.continuation = null
    
    // decide on what to do: 
    // activate next operand and/or have success, suspend, resume, exclude, or deactivate or nothing
    
    // decide on activate next operand
    
    var activateNextOrEnded       = false
    var activateNext              = false
    var activationEnded           = false
    var activationEndedOptionally = false
    var childNode: CallGraphNodeTrait[_<:TemplateNode] = null // may indicate node from which the a message came

    val isSequential = 
     n.template.kind match {
      case ";" | "|;"  | "||;" |  "|;|" => true
      case _ => false
     }

    if (n.activationMode!=ActivationMode.Inactive) {
     n.template.kind match {
      case "%" => val d = message.deactivations; 
                  val b = message.break
                  if (d!=Nil) {
                    activateNextOrEnded = d.size>0 && !d.head.excluded
                    if (activateNextOrEnded) {
                      childNode = d.head.node
                    }
                  }
      case ";" | "|;" 
       | "||;" |  "|;|" => // messy; maybe the outer if on the activationMode should be moved inside the match{}
                         val s = message.success
                         val b = message.break
                         if      (s!=null) {activateNextOrEnded = true; childNode = s.child}
                         else if (b!=null) {activateNextOrEnded = true; childNode = b.child
                           activationEndedOptionally = b.activationMode==ActivationMode.Optional 
                         }
      
      case "+" | "|+" | "|+|" 
                      => val a = message.aaActivated; val c = message.caActivated; val b = message.break
                         activateNextOrEnded = if (b==null) true
                                               else if (b.activationMode==ActivationMode.Optional) a!=null || c!=null
                                               else false 
                         if (activateNextOrEnded) {
                           childNode = n.lastActivatedChild         
                           //childNode = message.childNode         

                         }
                  
      case kind if (T_n_ary.isLeftMerge(kind)) => 
                         val aa = message.aaActivated
                         val ca = message.caActivated
                         val as = message.aaStarteds
                         val b  = message.break
                         activateNextOrEnded = aa==null && ca==null ||
                                               as!=Nil  && as.exists( (as:AAStarted) => as.node==n.lastActivatedChild )
                         if (b!=null) {
                           // ???
                         }
                         if (activateNextOrEnded) {
                           childNode = n.lastActivatedChild
                         }
                         
      case _          => if (message.activation!=null || message.aaStarteds!=Nil || message.success!=null || message.deactivations != Nil) {
                           val b  = message.break
                           val as = message.aaStarteds
                           n.aaStartedSinceLastOptionalBreak = n.aaStartedSinceLastOptionalBreak || as!=Nil
                           if (b==null||b.activationMode==ActivationMode.Optional) {
                             if (n.activationMode==ActivationMode.Optional) {
                                 activateNextOrEnded = n.aaStartedSinceLastOptionalBreak
                                 if (activateNextOrEnded) {
                                   n.activationMode = ActivationMode.Active
                                   n.aaStartedSinceLastOptionalBreak = false
                                   childNode = n.lastActivatedChild
                                 }
                             }
                             else if (message.activation!=null) {
                               activateNextOrEnded = true
                               childNode = n.lastActivatedChild
                             }
                           }
                         }
      }
    }
	var nextActivationTemplateIndex = 0
	var nextActivationPass = 0
	if (activateNextOrEnded) {
	  // old: childNode = if (T_n_ary.isLeftMerge(n.template.kind)) n.lastActivatedChild else message.childNode ; now done before
	  nextActivationTemplateIndex = childNode.template.indexAsChild+1
	  nextActivationPass = childNode.pass 
	  
	  message.node.activationMode = ActivationMode.Active
	  
	  if (n.hadBreak) activationEnded = true
	  else if (nextActivationTemplateIndex==message.node.template.children.size) {
	    if (message.node.isIteration) {
	      nextActivationTemplateIndex = 0
	      nextActivationPass += 1
	      activateNext = true
	    }
	    else {
	      activationEnded = true
	    }
	  }
	  else {
	    activateNext = true
	  }  
    }
    
    // decide on exclusions and suspensions; deciding on exclusions must be done before activating next operands, of course
    var nodesToBeExcluded : Buffer[CallGraphNodeTrait[_ <:TemplateNode]] = null
    var nodesToBeSuspended: Buffer[CallGraphNodeTrait[_ <:TemplateNode]] = null
    n.template.kind match {
                  
      case "/" | "|/" 
        | "|/|"        => // deactivate to the right when one has finished successfully
        				      // TBD: something goes wrong here; LookupFrame2 does not "recover" from a Cancel search
                          message.deactivations match {
                            case d::tail => if (d.child.hasSuccess && !d.excluded) {
                              nodesToBeExcluded = n.children.filter(_.index>d.child.index)
                            }
                            case _ =>
                          }
                  
      case "&&"  | "||" 
         | "&&:" | "||:" => val isLogicalOr = T_n_ary.getLogicalKind(n.template.kind)==LogicalKind.Or
                            val consideredNodes = message.deactivations.map(_.child).filter(
                               (c: CallGraphNodeTrait[_ <:TemplateNode]) => c.hasSuccess==isLogicalOr)
                            if (!consideredNodes.isEmpty) {
                              nodesToBeExcluded = n.children -- consideredNodes
                              activateNext = false
                            }
      case _ =>          
    }
    var shouldSucceed = false    
    // decide further on success and resumptions
    if (!shouldSucceed) { // could already have been set for .. as child of ;
      
      // TBD: improve
      if (activateNextOrEnded || message.success != null || message.aaEndeds != Nil) {
        var nodesToBeResumed: Buffer[CallGraphNodeTrait[_ <:TemplateNode]] = null
        if (message.success != null || message.aaEndeds != Nil) {
          n.template.kind match {
            case "/" => shouldSucceed = message.success != null ||
                                        message.aaEndeds.exists(_.child.index<n.rightmostChildThatEndedInSuccess_index) || 
		                                n.children.exists((e:CallGraphNodeTrait[_])=>e.hasSuccess)
            case _ =>
		          T_n_ary.getLogicalKind(n.template.kind) match {
		            case LogicalKind.None =>
		            case LogicalKind.And  => shouldSucceed = (isSequential || !n.aChildEndedInFailure && !activateNext) &&
		                                                     n.children.forall((e:CallGraphNodeTrait[_])=>e.hasSuccess)
		            case LogicalKind.Or   => shouldSucceed = n.aChildEndedInSuccess || 
		                                                     n.children.exists((e:CallGraphNodeTrait[_])=>e.hasSuccess)
                  }
          }
        }
      }
	}
    if (shouldSucceed && (
        !isSequential   ||   // TBD: check for other Sequential operators";"
        activationEnded || // reached the end
        message.break != null // succeed on sequential breaks, including the optional ones
      ) ) {
      insert(Success(n)) // TBD: prevent multiple successes at same "time"
    }
    // do exclusions and suspensions
    if (nodesToBeExcluded !=null) nodesToBeExcluded .foreach((n) => insert(Exclude(message.node,n)))
    if (nodesToBeSuspended!=null) nodesToBeSuspended.foreach((n) => insert(Suspend(n)))

    // do activation    
    
    if (activateNext) {
      val t = message.node.template.children(nextActivationTemplateIndex)
      activateFrom(message.node, t, Some(nextActivationPass))
      if (message.activation!=null) {
        val nary_op_isLeftMerge = n match {
          case nary@N_n_ary_op (t: T_n_ary, isLeftMerge) => isLeftMerge case _ => false
        }
        if (!nary_op_isLeftMerge) insertContinuation(message.activation, n)
      }
    }
    else if (n.children.isEmpty) {
      insertDeactivation(n, null)
    }
      
    // decide on deactivation of n
    
  }

  /*
   * message dispatcher; not really OO, but all real activity should be at the executors; other things should be passive
   */
  def handle(message: CallGraphMessage[_ <: subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode] ]):Unit = {
    message match {
      case a@ Activation        (_) => handleActivation   (a)
      case a@Continuation       (_) => handleContinuation (a)
      case a@Continuation1      (_) => handleContinuation1(a)
      case a@Deactivation  (_,_, _) => handleDeactivation (a)
      case a@Suspend            (_) => {}
      case a@Resume             (_) => {}
      case a@Exclude          (_,_) => handleExclude    (a)
      case a@Success          (_,_) => handleSuccess    (a)
      case a@Break        (_, _, _) => handleBreak      (a)
      case a@AAActivated      (_,_) => handleAAActivated(a)
      case a@CAActivated      (_,_) => handleCAActivated(a)
      case a@CAActivatedTBD     (_) => handleCAActivatedTBD(a)
      case a@AAStarted        (_,_) => handleAAStarted  (a)
      case a@AAEnded          (_,_) => handleAAEnded    (a)
      case a@AAExecutionFinished(_) => handleAAExecutionFinished(a)
      case a@AAToBeReexecuted   (_) => handleAAToBeReexecuted   (a)
      case a@AAToBeExecuted     (_) => handleAAToBeExecuted     (a)
      case CommunicationMatchingMessage => handleCommunicationMatchingMessage
    }
  }
  
  /*
   * Main method of BasicExecutioner
   * Handle all messages until there is nothing more to do:
   * 
   * ...
   * If the message queue is not empty
   *   dequeue the first one and handle it
   * else if the root node still has child nodes: do a synchronized wait
   * else break
   *   
   * TBD: check for deadlock situations in case of unmatching communications.
   * The condition for the synchronized wait should be tightened 
   */
  def run: ScriptExecutor = {
    activateFrom(anchorNode, anchorNode.t_callee)
    var isActive = true
    while (isActive) { // main execution loop
      
      if (callGraphMessageCount > 0) {
        val m = dequeueCallGraphMessage
        messageHandled(m)
        handle(m)
      }
      else if (!rootNode.children.isEmpty) {
        messageAwaiting
        synchronized { // TBD: there should also be a synchronized call in the CodeExecutors
          if (callGraphMessageCount==0) // looks stupid, but event may have happened&notify() may have been called during tracing
            synchronized {
              wait() // for an event to happen 
            }
        }
        // note: there may also be deadlock because of unmatching communications
        // so there should preferably be a check for the existence of waiting event handling actions
      }
      else {
        isActive = false
      }
    }
    this
  }
}
