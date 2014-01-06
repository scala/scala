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

// Executors that execute any call to Scala code in the application:
// code fragments, script calls, parameter checks, tests in if and while, annotations
trait CodeExecutor // to make an empty class file so that ant will not get confused
trait CodeExecutorTrait {
  // graph operations such as aaStarted may only be done when called from executor!
  var canceled = false // TBD(?) inspect this flag before execution
  def asynchronousAllowed: Boolean
  
  def doCodeExecution[R](code: ()=>R): R = {
    code.apply // for Atomic Actions, if, while, tiny, script calls etc, and also for onActivate etc
  }
  private def shouldNotBeCalledHere = throw new Exception("Illegal Call")
  def executeAA     : Unit                                      = shouldNotBeCalledHere // TBD: clean up class/trait hierarchy so that this def can be ditched
  def executeAA(lowLevelCodeExecutor: CodeExecutorTrait): Unit  = shouldNotBeCalledHere // TBD: clean up class/trait hierarchy so that this def can be ditched
  def afterExecuteAA: Unit                                      = shouldNotBeCalledHere // TBD: clean up class/trait hierarchy so that this def can be ditched
  def interruptAA   : Unit                                      = shouldNotBeCalledHere // TBD: clean up class/trait hierarchy so that this def can be ditched
  def n: CallGraphNodeTrait
  def scriptExecutor: ScriptExecutor
  def cancelAA = canceled=true 
}
case class TinyCodeExecutor(n: CallGraphNodeTrait, scriptExecutor: ScriptExecutor) extends CodeExecutorTrait  { // TBD: for while, {!!}, @:, script call
  val asynchronousAllowed = false
  override def interruptAA   : Unit  = {} // TBD: clean up class/trait hierarchy so that this def can be ditched
  override def doCodeExecution[R](code: ()=>R): R = super.doCodeExecution{
    code
    }
}
abstract class AACodeFragmentExecutor[N<:N_atomic_action](_n: N, _scriptExecutor: ScriptExecutor) extends CodeExecutorTrait  {
  
  // Executor for Atomic Actions. These require some communication with the ScriptExecutor, to make sure that 
  // graph messages such as AAStarted, AAEnded en Success are properly sent.
  // Note: such messages may only be handled from the main ScriptExecutor loop!
  //
  // Since scala code execution from Subscript may be asynchronous (e.g., in the Swing thread or in a new thread), 
  // there is some loosely communication with the ScriptExecutor
  // E.g., after calling the Scala code, the method executionFinished is called, which inserts an AAExecutionFinished
  def n = _n
  def scriptExecutor = _scriptExecutor
  val asynchronousAllowed = true
  override def interruptAA = {}
  override def cancelAA = super.cancelAA; interruptAA
  def naa = n.asInstanceOf[N]
  def doCodeExecution(lowLevelCodeExecutor: CodeExecutorTrait): Unit = lowLevelCodeExecutor.doCodeExecution{
    ()=>
      n.hasSuccess = true
      n.isExecuting = true
      try {
        // naa.template.code.apply.apply(naa) don't get this to work, so use a match statement:
        n.doCode // may affect n.hasSuccess
      } 
      finally {n.isExecuting = false}
      executionFinished
  }
  def aaStarted = scriptExecutor.insert(AAStarted(n,null))
  def aaEnded   = scriptExecutor.insert(AAEnded  (n,null)) 
  def succeeded = {
    scriptExecutor.insert(Success  (n,null)) 
  }
  override def executeAA: Unit = executeAA(this) // for Atomic Action execution...should ensure that executionFinished is called
  def executeAA(lowLevelCodeExecutor: CodeExecutorTrait): Unit // for Atomic Action execution...should ensure that executionFinished is called
  def afterExecuteAA             // to be called by executor, asynchronously, in reaction to executionFinished (through a message queue, not through a call inside a call)
  def executionFinished = scriptExecutor.insert(AAExecutionFinished(naa)) // so that executor calls afterRun here
  def toBeReexecuted    = scriptExecutor.insert(AAToBeReexecuted   (naa)) // so that executor reschedules n for execution
  def deactivate        = scriptExecutor.insert(Deactivation       (naa,null,false))
  def suspend   = {}
  def resume    = {}

  def notifyScriptExecutor = 
    scriptExecutor.synchronized {
      scriptExecutor.notify() // kick the scriptExecutor, just in case it was waiting
  }

}

class NormalCodeFragmentExecutor[N<:N_atomic_action](n: N, scriptExecutor: ScriptExecutor) extends AACodeFragmentExecutor[N](n, scriptExecutor)  {
  //without the next two definitions the compiler would give the following error messages; TBD: get rid of these
  // class NormalCodeFragmentExecutor needs to be abstract, since: 
  //   method scriptExecutor in trait CodeExecutorTrait of type => subscript.vm.ScriptExecutor is not defined 
  //   method n in trait CodeExecutorTrait of type => subscript.vm.CallGraphNodeTrait[_ <: subscript.vm.TemplateNode] is not defined	CodeExecutor.scala	/subscript/src/subscript/vm	line 54	Scala Problem
  
  
  override def executeAA(lowLevelCodeExecutor: CodeExecutorTrait): Unit = {
    aaStarted
    doCodeExecution(lowLevelCodeExecutor)
  }
  override def afterExecuteAA = {
    if (!n.isExcluded) {
       aaEnded; succeeded; deactivate
    }
  }
}
class UnsureCodeFragmentExecutor(n: N_code_unsure, scriptExecutor: ScriptExecutor) extends AACodeFragmentExecutor(n, scriptExecutor)  {
  override def executeAA(lowLevelCodeExecutor: CodeExecutorTrait): Unit = {
    doCodeExecution(lowLevelCodeExecutor)
  }
  override def afterExecuteAA = {
    if (n.hasSuccess) {
       aaStarted; aaEnded; succeeded; deactivate
    }
    else if (n.result==UnsureExecutionResult.Ignore){ // allow for deactivating result
      n.result=UnsureExecutionResult.Success
      toBeReexecuted
    }
    else { 
      deactivate
    }
  }
}
// Adapter to wrap around other CodeExecutors
// TBD: improve
trait CodeExecutorAdapter[CE<:CodeExecutorTrait] extends CodeExecutorTrait {
  var adaptee: CE = _
  def adapt[R](codeExecutor: CE) = {adaptee = codeExecutor}
  def asynchronousAllowed = adaptee.asynchronousAllowed
  def notifyScriptExecutor = adaptee.scriptExecutor.synchronized {
        adaptee.scriptExecutor.notify() // kick the scriptExecutor, just in case it was waiting
  }
}
class ThreadedCodeFragmentExecutor(n: N_code_threaded, scriptExecutor: ScriptExecutor) extends NormalCodeFragmentExecutor(n, scriptExecutor)  {
  override def interruptAA: Unit = if (myThread!=null) myThread.interrupt
  var myThread: Thread = null
  override def doCodeExecution(lowLevelCodeExecutor: CodeExecutorTrait): Unit = {
      val runnable = new Runnable {
        def run() {
          ThreadedCodeFragmentExecutor.super.doCodeExecution(lowLevelCodeExecutor)
          notifyScriptExecutor // kick the scriptExecutor, just in case it was waiting
        }
      }
      myThread = new Thread(runnable)
      myThread.start()
  }
}
class SwingCodeExecutorAdapter[CE<:CodeExecutorTrait] extends CodeExecutorAdapter[CE]{
  def n = adaptee.n
  def scriptExecutor = adaptee.scriptExecutor
  override def      executeAA: Unit = adaptee.executeAA(this) // Not to be called? TBD: clean up class/trait hierarchy 
  override def afterExecuteAA: Unit = adaptee.afterExecuteAA  // TBD: clean up class/trait hierarchy so that this def can be ditched
  override def    interruptAA: Unit = adaptee.interruptAA     // TBD: clean up class/trait hierarchy so that this def can be ditched
  override def doCodeExecution[R](code: ()=>R): R = {
    
    // we need here the default value for R (false, 0, null or a "Unit")
    // for some strange reason, the following line would go wrong:
    //
    // var result: R = _
    //
    // A solution using a temporary class was found at
    // http://missingfaktor.blogspot.com/2011/08/emulating-cs-default-keyword-in-scala.html
    class Tmp {var default: R = _} 
    var result: R = (new Tmp).default
    // luckily we have the default value for type R now...
    
    if (adaptee.asynchronousAllowed) {
      var runnable = new Runnable {
        def run(): Unit = {result = adaptee.doCodeExecution(code); notifyScriptExecutor}
      }
      javax.swing.SwingUtilities.invokeLater(runnable)
    }
    else {
      var runnable = new Runnable {
        def run(): Unit = {result = adaptee.doCodeExecution(code)}
      }
      javax.swing.SwingUtilities.invokeAndWait(runnable)
    }
    result
  }
}
case class EventHandlingCodeFragmentExecutor[N<:N_atomic_action](_n: N, _scriptExecutor: ScriptExecutor) extends AACodeFragmentExecutor(_n, _scriptExecutor)  {
  override def executeAA(lowLevelCodeExecutor: CodeExecutorTrait): Unit = executeMatching(true) // dummy method needed because of a flaw in the class hierarchy
  def executeMatching(isMatching: Boolean): Unit = {  // not to be called by scriptExecutor, but by application code
    n.hasSuccess = isMatching
    if (n.hasSuccess) 
    {
      n.doCode // may affect n.hasSuccess
    }
    if (n.hasSuccess)  // probably this test can be ditched
    {
      executionFinished // will probably imply a call back to afterExecute from the ScriptExecutor thread
                        // TBD: maybe a provision should be taken here to prevent handling a second event here, in case this is a N_code_eh
      notifyScriptExecutor // kick the scriptExecutor, just in case it was waiting
    }
  }
  override def afterExecuteAA: Unit = {
      if (n.isExcluded || !n.hasSuccess) return
      n match {
        case eh:N_code_eventhandling =>       
          aaStarted
          aaEnded
          succeeded
          deactivate 

        case eh:N_code_eventhandling_loop =>
             aaStarted
             aaEnded
             eh.result match {
                case LoopingExecutionResult.Success       => 
                case LoopingExecutionResult.Break         => succeeded
                case LoopingExecutionResult.OptionalBreak => succeeded; deactivate
             }
      }
  }
}
