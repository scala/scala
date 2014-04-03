package subscript

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import subscript.Predef._
import subscript.DSL._
import subscript.vm.{TemplateChildNode, N_code_unsure, CallGraphNodeTrait, UnsureExecutionResult, 
                     ScriptExecutor, CommonScriptExecutor, SimpleScriptDebugger}

class Test {
    
  def script..
   times(n:Int) = while(here.pass < n)

  val times1: Int=>Script = n=>{_script(this, 'lambda) {_while{implicit here=>pass<n}}}
  val times2: Int=>Script = n=> [ while(here.pass < n) ]
  //val times3: Int=>Script =     [ while(pass < _) ]
  //val noXml = [noXML]

  //TBD: allow ? in all parameter lists; parse < script > blocks
}