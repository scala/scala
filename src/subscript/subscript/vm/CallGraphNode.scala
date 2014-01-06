/*
    This file is part of Subscript - an extension of the Scala langutage 
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
 * Classes for representing run time nodes in the Call Graph.
 * 
 * TBD: rethink inheritance scheme. Maybe use traits exclusively for inheritance
 */

object ActivationMode extends Enumeration {
  type ActivationModeType = Value
  val Active, Optional, Inactive = Value
}

object UnsureExecutionResult extends Enumeration {
  type UnsureExecutionResultType = Value
  val Success, Failure, Ignore = Value
}

object LoopingExecutionResult extends Enumeration {
  type LoopingExecutionResultType = Value
  val Success, Failure, Break, OptionalBreak = Value
}

object OnActivate
object OnActivateOrResume
object OnDeactivate
object OnDeactivateOrSuspend
object OnSuccess
 
trait CallGraphNodeTrait {
  type T <: TemplateNode
  var hasSuccess = false
  var isExcluded = false
  var pass = 0
  def template: T
  def n_ary_op_else_ancestor: N_n_ary_op
  def lowestSingleCommonAncestor: CallGraphParentNodeTrait
  def forEachParent(n: CallGraphParentNodeTrait => Unit): Unit
  def isExecuting  = false
  var numberOfBusyActions = 0
  def isActionBusy = numberOfBusyActions>0

  var index = -1
  var stamp = 0
  var aaStartedCount = 0
  var properties: Map[Any,Any] = new HashMap[Any,Any]
  var _scriptExecutor: ScriptExecutor = null
  def scriptExecutor = _scriptExecutor
  def scriptExecutor_=(s: ScriptExecutor) = {
    index = s.nextNodeIndex()
    _scriptExecutor = s
  }
  
  def getCodeProperty(key: Any): ()=>Unit = {
    properties.get(key) match {
      case None => null
      case Some(cp) => cp.asInstanceOf[()=>Unit]
    }
  }
  def setCodeProperty(key: Any, c: ()=>Unit) = {
    properties += key->c
  }
  def onActivate             : ()=>Unit  = getCodeProperty(OnActivate                  )
  def onActivateOrResume     : ()=>Unit  = getCodeProperty(OnActivateOrResume          )
  def onDeactivate           : ()=>Unit  = getCodeProperty(OnDeactivate                )
  def onDeactivateOrSuspend  : ()=>Unit  = getCodeProperty(OnDeactivateOrSuspend       )
  def onSuccess              : ()=>Unit  = getCodeProperty(OnSuccess                   )
  def onActivate           (c:   =>Unit) = setCodeProperty(OnActivate           , ()=>c)
  def onActivateOrResume   (c:   =>Unit) = setCodeProperty(OnActivateOrResume   , ()=>c)
  def onDeactivate         (c:   =>Unit) = setCodeProperty(OnDeactivate         , ()=>c)
  def onDeactivateOrSuspend(c:   =>Unit) = setCodeProperty(OnDeactivateOrSuspend, ()=>c)
  def onSuccess            (c:   =>Unit) = setCodeProperty(OnSuccess            , ()=>c)

  def asynchronousAllowed: Boolean = false
  var codeExecutor: CodeExecutorTrait = null
  def adaptExecutor(ca: CodeExecutorAdapter[CodeExecutorTrait]): Unit = {ca.adapt(codeExecutor); codeExecutor=ca}
  var aChildEndedInFailure = false
  def aChildEndedInSuccess = rightmostChildThatEndedInSuccess_index>=0
  def childThatEndedInSuccess_index(i: Int) = rightmostChildThatEndedInSuccess_index = 
             if (rightmostChildThatEndedInSuccess_index== -1) i else scala.math.max(rightmostChildThatEndedInSuccess_index, i)
                                  
  var rightmostChildThatEndedInSuccess_index = -1

  override def toString = index+" "+template

  // TBD: are these necessary? :
  //var aaStartedCountAtLastSuccess = 0
  //def recentSuccess = aaStartedCountAtLastSuccess==aaStartedCount
}

// a non-leaf node:
trait CallGraphParentNodeTrait extends CallGraphNodeTrait {
  val children = new ListBuffer[CallGraphNodeTrait]
  def forEachChild(task: (CallGraphNodeTrait) => Unit): Unit = {
    children.foreach(task(_))
  }
}

trait DoCodeHolder[R] {
  def doCode: R
  private var _isExecuting = false
  override def isExecuting = _isExecuting
           def isExecuting_=(value: Boolean) = _isExecuting=value
}

trait CallGraphNode extends CallGraphNodeTrait {
  
  def n_ary_op_else_ancestor: N_n_ary_op = {
    this match {
      case n:N_n_ary_op => n
      case _            => n_ary_op_ancestor
    }
  }
  // answer the n_ary_op ancestor in case there is one and the path leading thereto does not branch
  def n_ary_op_ancestor: N_n_ary_op
  def n_ary_op_ancestor_up(n: Int): N_n_ary_op = {
    var ancestor = n_ary_op_ancestor
    if (n<=0 || ancestor==null) return ancestor
    return ancestor.n_ary_op_ancestor_up(n-1)
  }
  def break_up0 = break_up(0)
  def break_up1 = break_up(1)
  def break_up2 = break_up(2)
  def break_up3 = break_up(3)
  def break_up(n: Int): Unit = {
    var ancestor = n_ary_op_ancestor_up(n)
    if (ancestor!=null) ancestor.mustBreak
  }
  def getLogicalKind_n_ary_op_ancestor: LogicalKind.LogicalKindType = {
    val a = n_ary_op_ancestor
    if (a==null) return null
    a.getLogicalKind
  }
}


// a node that may have at most 1 parent; always used, except for rendez-vous style communication
// should this not be an abstract class?
trait CallGraphTreeNode extends CallGraphNode {
  var parent: CallGraphParentNodeTrait = null
  // answer the n_ary_op ancestor in case there is one and the path leading thereto does not branch
  def n_ary_op_ancestor = if (parent==null) null else parent.n_ary_op_else_ancestor
  def lowestSingleCommonAncestor = parent
  def forEachParent(task: CallGraphParentNodeTrait => Unit): Unit = {
    if (parent!=null) {
      task(parent)
    }
  }
  def passToBeUsedToGetVariableNamed(name: Symbol) = pass
  def getLocalVariableHolder[V<:Any](name: Symbol): VariableHolder[V] = {
    //var usePass = this match {
    //  case lvl@N_localvar_loop(_:T) if (lvl.name==name) => pass-1
    //  case _ => pass
    //}
    // yields strange compile error message:
    // constructor cannot be instantiated to expected type;  found   : subscript.vm.N_localvar_loop[V]  required: subscript.vm.CallGraphTreeNode[T]	
    // so the following code is used instead:

    var usePass = passToBeUsedToGetVariableNamed(name)
    var result: VariableHolder[V] = null
    var nary   = n_ary_op_ancestor
    while (result==null) {
      result = nary.getVariableHolder(name, usePass)
      if (result==null) {
        usePass = nary.pass
        nary    = nary.n_ary_op_ancestor
      }
    }
    result
  }
}
// a node that may have multiple parents; used for rendez-vous style communication
// should this not be an abstract class?
trait CallGraphNonTreeNode extends CallGraphNode {
  val parents = new LinkedList[CallGraphParentNodeTrait]
  var lowestSingleCommonAncestor: CallGraphParentNodeTrait = null

  // answer the n_ary_op ancestor in case there is one and the path leading thereto does not branch
  def n_ary_op_ancestor: N_n_ary_op = null
  def forEachParent(task: CallGraphParentNodeTrait => Unit): Unit = {
    parents.foreach(task(_))
  }
}

trait CallGraphLeafNode          extends CallGraphTreeNode    {var queue: Buffer[CallGraphNodeTrait] = null}
trait CallGraphTreeParentNode    extends CallGraphTreeNode    with CallGraphParentNodeTrait {}
trait CallGraphNonTreeParentNode extends CallGraphNonTreeNode with CallGraphParentNodeTrait {}

// name not 100% appropriate: many "AA"'s are not really atomic, and N_code_tiny is not a real action
abstract class N_atomic_action  extends CallGraphLeafNode with DoCodeHolder[Unit] {
  type T <: T_atomic_action
  override def asynchronousAllowed: Boolean = true
  var msgAAToBeExecuted: CallGraphMessage[_] = null
}

abstract class CallGraphTreeNode_n_ary extends CallGraphTreeParentNode {
  type T <: T_n_ary_op
  var isIteration      = false
  var hadBreak         = false
  var activationMode = ActivationMode.Active
  def getLogicalKind = T_n_ary_op.getLogicalKind(template)
  var continuation: Continuation = null
  var lastActivatedChild: CallGraphNodeTrait = null
  var aaStartedSinceLastOptionalBreak = false
  def mustBreak = hadBreak = true
}

// The case classes for the bottom node types

case class N_code_normal     (template: T_code_normal  ) extends N_atomic_action {type T = T_code_normal  ; def doCode = template.code.apply.apply(this)}
case class N_code_tiny       (template: T_code_tiny    ) extends N_atomic_action {type T = T_code_tiny    ; def doCode = template.code.apply.apply(this)}
case class N_code_threaded   (template: T_code_threaded) extends N_atomic_action {type T = T_code_threaded; def doCode = template.code.apply.apply(this)}
case class N_code_unsure     (template: T_code_unsure  ) extends N_atomic_action {type T = T_code_unsure  ; def doCode = template.code.apply.apply(this)
  private var _result = UnsureExecutionResult.Success; // TBD: clean this all up; hasSuccess+result is too much
  def result = _result
  def result_=(value: UnsureExecutionResult.UnsureExecutionResultType): Unit = {
    _result = value
    hasSuccess = value==UnsureExecutionResult.Success
  }
}
case class N_code_eventhandling         (template: T_code_eventhandling     ) extends N_atomic_action {type T = T_code_eventhandling     ; def doCode = template.code.apply.apply(this)}
case class N_code_eventhandling_loop    (template: T_code_eventhandling_loop) extends N_atomic_action {type T = T_code_eventhandling_loop; def doCode = template.code.apply.apply(this)
  private var _result = LoopingExecutionResult.Success; 
  def result = _result
  def result_=(value: LoopingExecutionResult.LoopingExecutionResultType): Unit = {
    _result = value
    hasSuccess = value==LoopingExecutionResult.Success || value==LoopingExecutionResult.Break || value==LoopingExecutionResult.OptionalBreak
  }
}
case class N_localvar[V](template: T_localvar[V]) extends CallGraphLeafNode with DoCodeHolder[V] {
  type T = T_localvar[V]
  def doCode = template.code.apply.apply(this)
  override def passToBeUsedToGetVariableNamed(thatName: Symbol): Int = if (template.isLoop&&template.localVariable.name==thatName) pass-1 else pass // used in: var i=0...(i+1)
}
case class N_privatevar         (template: T_privatevar         ) extends CallGraphLeafNode                                  {type T = T_privatevar         }
case class N_while              (template: T_while              ) extends CallGraphLeafNode with DoCodeHolder[Boolean]       {type T = T_while              ; def doCode = template.code.apply.apply(this)}
case class N_break              (template: T_break              ) extends CallGraphLeafNode                                  {type T = T_break              }
case class N_optional_break     (template: T_optional_break     ) extends CallGraphLeafNode                                  {type T = T_optional_break     }
case class N_optional_break_loop(template: T_optional_break_loop) extends CallGraphLeafNode                                  {type T = T_optional_break_loop}
case class N_delta              (template: T_delta              ) extends CallGraphLeafNode                                  {type T = T_delta              }
case class N_epsilon            (template: T_epsilon            ) extends CallGraphLeafNode                                  {type T = T_epsilon            }
case class N_nu                 (template: T_nu                 ) extends CallGraphLeafNode                                  {type T = T_nu                 }
case class N_loop               (template: T_loop               ) extends CallGraphLeafNode                                  {type T = T_loop               }
case class N_1_ary_op           (template: T_1_ary_op           ) extends CallGraphTreeParentNode                            {type T = T_1_ary_op           ; var continuation: Continuation1 = null}
case class N_if                 (template: T_if                 ) extends CallGraphTreeParentNode with DoCodeHolder[Boolean] {type T = T_if                 ; def doCode = template.code.apply.apply(this)}
case class N_if_else            (template: T_if_else            ) extends CallGraphTreeParentNode with DoCodeHolder[Boolean] {type T = T_if_else            ; def doCode = template.code.apply.apply(this)}
case class N_launch             (template: T_launch             ) extends CallGraphLeafNode                                  {type T = T_launch             }

case class N_annotation[CN<:CallGraphNodeTrait,CT<:TemplateChildNode] (template: T_annotation[CN,CT]) extends CallGraphTreeParentNode with DoCodeHolder[Unit] {
  type T = T_annotation[CN,CT]
  def there:CN=children.head.asInstanceOf[CN]
  def doCode = template.code.apply.apply(this)
}

// the following 4 types may have multiple children active synchronously
case class N_launch_anchor      (template: T_launch_anchor                       ) extends CallGraphTreeParentNode {type T = T_launch_anchor }
case class N_inline_if          (template: T_inline_if                           ) extends CallGraphTreeParentNode {type T = T_inline_if     }
case class N_inline_if_else     (template: T_inline_if_else                      ) extends CallGraphTreeParentNode {type T = T_inline_if_else}
case class N_n_ary_op           (template: T_n_ary_op      , isLeftMerge: Boolean) extends CallGraphTreeNode_n_ary {type T = T_n_ary_op      
  val mapNamePassToVariableHolder = new HashMap[(Symbol,Int), VariableHolder[_]]
  def    initLocalVariable[V<:Any](name: Symbol, fromPass: Int, value: V)         = mapNamePassToVariableHolder += ((name,fromPass)->new VariableHolder(value))
  def    getVariableHolder[V<:Any](name: Symbol, fromPass: Int):VariableHolder[V] = mapNamePassToVariableHolder.get((name,fromPass)) match {
                                                                                      case None=>null 
                                                                                      case Some(v:VariableHolder[V]) => v 
                                                                                      case _ => throw new Exception("not matched")}
  override def toString = super.toString+/*" "+children.length+*/(if(isIteration)" ..."else"")
}

// only one class for normal script calls and communicator-script calls
// this will make parsing etc much easier,
// but there are some fields that are either for normal script calls or for communicator-script calls
case class N_call(template: T_call) extends CallGraphTreeParentNode with DoCodeHolder[Unit] {
  type T = T_call
  var t_callee    : T_script     = null
  var t_commcallee: T_commscript = null
  def doCode = {
    var v = template.code.apply.apply(this)
    v(this)
  }
  def communicator: Communicator = if (t_commcallee==null) null else t_commcallee.communicator
  def stopPending {if (communicator!=null) {communicator.removePendingCall(this)}}
  var actualParameters: scala.collection.immutable.Seq[ActualParameter[_<:Any]] = Nil
  def calls(t: T_script, args: FormalParameter[_]*): Unit = {
    this.actualParameters = args.toList.map(_.asInstanceOf[ActualParameter[_]])
    this.t_callee = t
  }
  def calls(t: T_commscript, args: FormalParameter[_]*): Unit = {
    this.actualParameters = args.toList.map(_.asInstanceOf[ActualParameter[_]])
    this.t_commcallee = t
  }
  def allActualParametersMatch: Boolean = actualParameters.forall {_.matches}
  def transferParameters      : Unit    = actualParameters.foreach{_.transfer}
}
case class N_script       (var template: T_script       ) extends CallGraphTreeParentNode    {type T = T_script}
case class N_communication(var template: T_communication) extends CallGraphNonTreeParentNode {type T = T_communication
  def inits(t: T_communication, owner: Any): TemplateNode = {return null // TBD
  }
  def _getParameter[T](p: Symbol): CommunicationParameter[T] = {return CommunicationParameter[T](p)}
  var communication: Communication = null
}

// several utility stuffs

object Multiplicity extends Enumeration {
  type MultiplicityType = Value
  val Zero_or_One, Zero_or_More, One, One_or_More = Value
}


// no nodes, but structures to support communications
case class Communication(_body: N_communication => TemplateNode) {
  var communicatorRoles: List[CommunicatorRole] = null
  var template: T_communication = null
  def setCommunicatorRoles(crs: List[CommunicatorRole]): Unit = {
    var names = new ListBuffer[Symbol]
    communicatorRoles = crs
    for (cr <- crs) {
      cr.communication = this
      cr.communicator.roles += cr
      names += cr.communicator.name
    }
    template = T_communication(null, "comm", names)
  }
}
case class Communicator(name: Symbol) {
  def removePendingCall(call: N_call) {instances -= call}
  val instances = scala.collection.mutable.ArrayBuffer.empty[N_call]
  var roles = new ListBuffer[CommunicatorRole]
}
case class CommunicatorRole(communicator: Communicator) {
  var communication: Communication = null
  var multiplicity = Multiplicity.One
  var parameterNames = new ListBuffer[Symbol] 
  def ~(s: Symbol): CommunicatorRole = {parameterNames += s; this}
  def ~(m: Multiplicity.MultiplicityType): CommunicatorRole = {multiplicity = m; this}
}

// Utility stuff for Script Call Graph Nodes
object CallGraphNode {
  var currentStamp = 0; // used for searching common ancestors
  
  def nextStamp() = {currentStamp = currentStamp+1; currentStamp}
  
  // answer the stepsUp'th N_n_ary_op ancestor node
  def upInGraphToNAry(n: CallGraphTreeNode) : N_n_ary_op = {
    var a = n
    while (true) {
      a match {
        case nary: N_n_ary_op => return nary
        case _ =>
      }
      a = a.parent.asInstanceOf[CallGraphTreeNode]
    }
    return null // dummy exit
  }
  
  // find the lowest launch_anchor common ancestor of a node
  //
  def getLowestLaunchAnchorAncestor(n: CallGraphNodeTrait) = 
      getLowestSingleCommonAncestor(n, _.isInstanceOf[N_launch_anchor] )
      
  // find the lowest single common ancestor of a node, that fulfills a given condition:
  // easy when there is 0 or 1 parents
  //
  def getLowestSingleCommonAncestor(n: CallGraphNodeTrait, condition: (CallGraphNodeTrait)=>Boolean): CallGraphParentNodeTrait = {
    val lsca = n.lowestSingleCommonAncestor
    if (lsca==null) return null
    if (condition(lsca)) return lsca
    return getLowestSingleCommonAncestor(lsca,condition)
  }
  
  private def stampNodeWithAncestors(n: CallGraphNodeTrait): Unit = {
    if (n.stamp==currentStamp) {
      // this one has already been stamped this round, so here branches come together
      // maybe it is the oldest of such nodes thus far; then record it
      if (lowestSingleCommonAncestor==null
      ||  lowestSingleCommonAncestor.index > n.index)
      {
        lowestSingleCommonAncestor = n
      }
    }
    else
    {
      n.stamp = currentStamp;
      n.forEachParent(stampNodeWithAncestors)
    }
  }
  
  private var lowestSingleCommonAncestor: CallGraphNodeTrait = null
  
  // find the lowest common ancestor of a collection of nodes:
  // for each node, stamp upwards in the graph; 
  // each time when the current stamp is encountered, that node may be the lowest common ancestor
  // the oldest of such candidates is considered the one.
  //
  // NOTE: this will return a false LCA when the true LCA has multiple paths to the graph source!!!
  private def getLowestSingleCommonAncestor(nodes: List[CallGraphNode]): CallGraphNodeTrait = {
    nextStamp() 
    lowestSingleCommonAncestor = null
    nodes.foreach(stampNodeWithAncestors(_))
    return lowestSingleCommonAncestor
  }
}
