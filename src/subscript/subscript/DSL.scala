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

package subscript

import scala.language.implicitConversions
import scala.collection.mutable.LinkedList
import subscript.vm._

/*
 * Internal Scala DSL for SubScript.
 * Using this DSL one can make SubScript programs without the need 
 * for a compiler that understands the specific SubScript syntax. 
 * 
 * Also this DSL may well be the target for a SubScript extension to the Scala compiler.
 * 
 * Usage: see example programs
 */
object DSL {
  type _scriptType = N_call=>Unit
  def _script   (owner : AnyRef, name        : Symbol      , p: FormalParameter[_]*)(_t: TemplateChildNode): _scriptType = {(_c: N_call) => _c.calls(T_script    (owner, "script"       , name,     _t), p:_*)}
  def _comscript(owner : AnyRef, communicator: Communicator, p: FormalParameter[_]*)                       : _scriptType = {(_c: N_call) => _c.calls(T_commscript(owner, "communicator" , communicator), p:_*)}
  
// TBD: communication scripts
//  def _communication(owner: Any, names: Symbol*): N_communication => TemplateNode = {
//    (_c: N_communication) => _c.inits(T_communication("communication", names.toList.map(_.asInstanceOf[Symbol])), owner)
//  }
//  def _communication(owner: Any, names: Symbol*)(_body: N_communication => TemplateNode) = { 
//    (_c: N_communication) => _c.inits(T_communication("communication", names.toList.map(_.asInstanceOf[Symbol])), owner)
//  }

  def _communication(body: N_communication => TemplateNode) = Communication(body)
  def _communicator(name: Symbol) = Communicator(name)
  def _relate(communication: Communication, crs: CommunicatorRole*): Unit = communication.setCommunicatorRoles(crs.toList)

  implicit def communicatorToCommunicatorRole(c: Communicator) = new CommunicatorRole(c)
  
  def _execute(_script: N_call => Unit): ScriptExecutor = _execute(_script, null, true)
  def _execute(_script: N_call => Unit, executor: ScriptExecutor): ScriptExecutor = _execute(_script, null, executor)
  def _execute(_script: N_call => Unit, debugger: ScriptDebugger): ScriptExecutor = _execute(_script, debugger, false)
  def _execute(_script: N_call => Unit,                           allowDebugger: Boolean): ScriptExecutor = _execute(_script, null, allowDebugger)
  def _execute(_script: N_call => Unit, debugger: ScriptDebugger, allowDebugger: Boolean): ScriptExecutor = {
    val executor = ScriptExecutorFactory.createScriptExecutor(allowDebugger && debugger == null)
    _execute(_script, debugger, executor)
  }
  def _execute(_script: N_call => Unit, debugger: ScriptDebugger, executor: ScriptExecutor): ScriptExecutor = {
    if (debugger!=null) debugger.attach(executor)
    _script(executor.anchorNode)
    executor.run
  }

  implicit // these code fragment variations require the "here" parameter explicitly
  def _normal             (cf: => (N_code_normal             =>Unit)) = T_code_normal            (() => cf)
  def _threaded           (cf: => (N_code_threaded           =>Unit)) = T_code_threaded          (() => cf)
  def _unsure             (cf: => (N_code_unsure             =>Unit)) = T_code_unsure            (() => cf)
  def _tiny               (cf: => (N_code_tiny               =>Unit)) = T_code_tiny              (() => cf)
  def _eventhandling      (cf: => (N_code_eventhandling      =>Unit)) = T_code_eventhandling     (() => cf)
  def _eventhandling_loop (cf: => (N_code_eventhandling_loop =>Unit)) = T_code_eventhandling_loop(() => cf)

  implicit // alternative code fragment variations that have no "here" parameter
  def _normal0            (cf: => Unit) = T_code_normal            (() => (_here:N_code_normal            ) => cf)
  def _threaded0          (cf: => Unit) = T_code_threaded          (() => (_here:N_code_threaded          ) => cf)
  def _unsure0            (cf: => Unit) = T_code_unsure            (() => (_here:N_code_unsure            ) => cf)
  def _tiny0              (cf: => Unit) = T_code_tiny              (() => (_here:N_code_tiny              ) => cf)
  def _eventhandling0     (cf: => Unit) = T_code_eventhandling     (() => (_here:N_code_eventhandling     ) => cf)
  def _eventhandling_loop0(cf: => Unit) = T_code_eventhandling_loop(() => (_here:N_code_eventhandling_loop) => cf)

  implicit def _call      (cf: => (N_call                    =>Unit)) = T_call(()=>n=>cf)
  
  implicit def valueToActualValueParameter[T<:Any](value: T) = new ActualValueParameter(value)

  def _at[N<:CallGraphNodeTrait,T<:TemplateChildNode](_cf:N=>Unit)  
  = (_child: T) => T_annotation[N,T](() => (here:N_annotation[N,T]) => _cf(here.there), _child)
 
  def _declare[T](name: Symbol) = new LocalVariable[T](name)
  
  // local variables need to be declared explicitly first; usage is as in:
  //  implicit def _key(_publisher: FormalInputParameter[Publisher], _keyCode: FormalConstrainedParameter[Char])  = {
  //    val _r = _declare[KeyPressScriptReactor[N_code_eh]]('r)      // <<== declaration
  //    _script('key, _publisher~'publisher, _keyCode~??'keyCode) {
  //     _seq( 
  //       _val(_r, (here:N_localvar[_]) => new KeyPressScriptReactor[N_code_eh](_publisher.value, _keyCode)),  // <<== initialisation
  //       _at{(there:N_code_eh) => {_r.at(there).value.subscribe(there); there.onDeactivate{_r.at(there).value.unsubscribe}; 
  //                                                                      there.onSuccess   {_r.at(there).value.acknowledgeEventHandled}}}
  //          (_eventhandling{})//{println("\nKey"+_keyCode.value)} // Temporary tracing
  //     )
  //    }
  //  }
  
  def _var     [V<:Any](v: LocalVariable[V]                                ) = T_localvar(false, false, v, null)
  def _var_loop[V<:Any](v: LocalVariable[V]                                ) = T_localvar(false,  true, v, null)
  def _var     [V<:Any](v: LocalVariable[V], valueCode: => N_localvar[V]=>V) = T_localvar(false, false, v, () => valueCode)
  def _val     [V<:Any](v: LocalVariable[V], valueCode: => N_localvar[V]=>V) = T_localvar( true, false, v, () => valueCode)
  def _var_loop[V<:Any](v: LocalVariable[V], valueCode: => N_localvar[V]=>V) = T_localvar(false,  true, v, () => valueCode)
  def _val_loop[V<:Any](v: LocalVariable[V], valueCode: => N_localvar[V]=>V) = T_localvar( true,  true, v, () => valueCode)

  def _privatevar[T<:Any](vsym: Symbol) = T_privatevar(vsym)

  // variants for operators with 0 to many operands
  //def _op0(opSymbol: String)                                                                      = T_0_ary(opSymbol)
  //def _op1(opSymbol: String)(c0: TemplateChildNode)                                               = T_1_ary(opSymbol, c0)
  //def _op2(opSymbol: String)(c0: TemplateChildNode, c1: TemplateChildNode)                        = T_2_ary(opSymbol, c0, c1)
  //def _op3(opSymbol: String)(c0: TemplateChildNode, c1: TemplateChildNode, c2: TemplateChildNode) = T_3_ary(opSymbol, c0, c1, c2)

  /* the following does not function well, as of Scala 2.10.
   * See https://issues.scala-lang.org/browse/SI-4176
   *
  def _op (opSymbol: String)(children: TemplateChildNode*)                                        = T_n_ary(opSymbol, children:_*)
  
  def _seq               = _op(";")_
  def _alt               = _op ("+")_
  def _par               = _op ("&")_
  def _par_or            = _op ("|")_
  def _par_and2          = _op ("&&")_
  def _par_or2           = _op ("||")_
  def _par_equal         = _op ("==")_
  def _disrupt           = _op ("/")_
  def _shuffle           = _op ("%")_
  def _shuffle_1_or_more = _op ("%%")_
  def _seq_1_or_more     = _op (";%;")_
  def _interrupt         = _op ("%/")_
  def _interrupt_0_or_more = _op ("%/%/")_
  */
  
  def _op1(opSymbol: String)(child0  : TemplateChildNode ) = T_1_ary_op(opSymbol, child0)
  def _op (opSymbol: String)(children: TemplateChildNode*) = T_n_ary_op(opSymbol, children:_*)
  
  def _seq                (children: TemplateChildNode*) = _op(";"   )(children:_*)
  def _alt                (children: TemplateChildNode*) = _op("+"   )(children:_*)
  def _par                (children: TemplateChildNode*) = _op("&"   )(children:_*)
  def _par_or             (children: TemplateChildNode*) = _op("|"   )(children:_*)
  def _par_and2           (children: TemplateChildNode*) = _op("&&"  )(children:_*)
  def _par_or2            (children: TemplateChildNode*) = _op("||"  )(children:_*)
  def _par_equal          (children: TemplateChildNode*) = _op("=="  )(children:_*)
  def _disrupt            (children: TemplateChildNode*) = _op("/"   )(children:_*)
  def _shuffle            (children: TemplateChildNode*) = _op("%"   )(children:_*)
  def _shuffle_1_or_more  (children: TemplateChildNode*) = _op("%%"  )(children:_*)
  def _seq_1_or_more      (children: TemplateChildNode*) = _op(";%;" )(children:_*)
  def _interrupt          (children: TemplateChildNode*) = _op("%/"  )(children:_*)
  def _interrupt_0_or_more(children: TemplateChildNode*) = _op("%/%/")(children:_*)
  
  
  def _not               = _op1("!")_
  def _not_react         = _op1("-")_
  def _react             = _op1("~")_
  def _launch            = _op1("*")_
  def _launch_anchor     = _op1("**")_

  def _empty                                = T_epsilon            ()
  def _deadlock                             = T_delta              ()
  def _neutral                              = T_nu                 ()
  def _break                                = T_break              ()
  def _optionalBreak                        = T_optional_break     ()
  def _optionalBreak_loop                   = T_optional_break_loop()
  def _loop                                 = T_loop               ()
//def _if_inline                            = T_inline_if          () TBD
//def _if_else_inline                       = T_inline_if_else     () TBD
  def _while0  (_cond:         =>Boolean)   = T_while(() => (here: N_while ) => _cond)
  def _while   (_cond:N_while  =>Boolean)   = T_while(() =>                     _cond)
  def _if0     (_cond:         =>Boolean)(c0: TemplateChildNode) = T_if(() => (here: N_if) => _cond, c0)
  def _if      (_cond:N_if     =>Boolean)(c0: TemplateChildNode) = T_if(() =>                 _cond, c0)
  def _if_else0(_cond:         =>Boolean)(c0: TemplateChildNode, c1: TemplateChildNode) = T_if_else(() => (here: N_if_else) => _cond, c0, c1)
  def _if_else (_cond:N_if_else=>Boolean)(c0: TemplateChildNode, c1: TemplateChildNode) = T_if_else(() =>                      _cond, c0, c1)
 }