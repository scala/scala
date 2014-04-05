/*")private,) 
    This file is part of Subscript - anv extension of the Scala lanvaguage 
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

/*
 *  Template Nodes are used to describe abstract syntax trees of the compiled scripts
 */
import CallGraphNode._scriptType

object TemplateNode {
  

  def kindAsString(t: TemplateNode): String = 
    t match {
      // matching on T_n_ary_op (and T_1_ary_op) does not work;
      // therefore FTTB those classes have their own implementation of kindAsString
      case T_1_ary_op(kind: String, _) => kind
      case T_n_ary_op(kind: String, _) => kind
      
      case T_code_normal            (_) => "{}"
      case T_code_tiny              (_) => "{!!}"
      case T_code_threaded          (_) => "{**}"
      case T_code_unsure            (_) => "{??}"
      case T_code_eventhandling     (_) => "{.}"
      case T_code_eventhandling_loop(_) => "{...}"
      case T_localvar(isVal: Boolean, isLoop: Boolean, localVariable, _) => "var"
      case T_privatevar  (name: Symbol) => "private "+name
      case T_while                  (_) => "while"
      case T_break                   () => "break"
      case T_optional_break          () => "."
      case T_optional_break_loop     () => ".."
      case T_delta                   () => "(-)"
      case T_epsilon                 () => "(+)"
      case T_nu                      () => "(+-)"
      case T_loop                    () => "..."
      case T_if                   (_,_) => "if"
      case T_if_else            (_,_,_) => "if-else"
      case T_launch                 (_) => "*"
      case T_launch_anchor          (_) => "**"
      case T_then                 (_,_) => "then"
      case T_then_else          (_,_,_) => "then-else"
      case T_annotation           (_,_) => "@:"
      case T_call        (calleeName,_) => calleeName
      case T_script (_, kind: String, name: Symbol, _)          => name.toString
      case T_commscript(_, kind: String, _)                     => "cscript"
      case T_communication(_, kind: String, names: Seq[Symbol]) => "comm"
      case T_local_valueCode  (_, _, _)                         => "T_local_valueCode???"
      case _ => getClass.getName
    }
  
    def subScriptInfixOpPrecedence(operator: String): Int = // pretty compatible with Scala
      operator.charAt(0) match {
      case ';'             => 1
      case '|'             => 2
      case '^'             => 3
      case '&'             => 4
      case '=' | '!'       => 5
      case '<' | '>'       => 6
      case ':'             => 7
      case '+' | '-'       => 8
      case '*' | '/' | '%' => 9
      case _               => 10
    }
    
  private def hierarchyString(thisNode:TemplateNode, parent:TemplateNode, parentIsSpaceSeq: Boolean): String = 
  { 
    val children = thisNode.children
    
    // the next 13 lines seem to be necessary since matching on T_n_ary_op (and T_1_ary_op) does not work
    if (thisNode.isInstanceOf[T_n_ary_op]) {
        val tn = thisNode.asInstanceOf[T_n_ary_op]
        var isSpaceSeq = false
        val doParenthesize = if (parent==null) false 
           else if (parent.isInstanceOf[T_n_ary_op]) {
             val pn = parent.asInstanceOf[T_n_ary_op]
             if (tn.kind==";" && !parentIsSpaceSeq) {isSpaceSeq = true; false}
             else subScriptInfixOpPrecedence(tn.kind) <= subScriptInfixOpPrecedence(pn.kind)
           }
           else parent.isInstanceOf[T_annotation[_,_]] ||
                parent.isInstanceOf[T_1_ary_op]
        val s = children.map(hierarchyString(_, thisNode, isSpaceSeq)).mkString(if (isSpaceSeq) " " else thisNode.kindAsString)
        if (doParenthesize) "(" + s + ")" else s
    }
    else thisNode match {
      case T_n_ary_op(kind: String, _) =>
        var isSpaceSeq = false
        val doParenthesize = if (parent==null) false else parent match {
          case T_annotation        (_,_) => true
          case T_launch              (_) => true
          case T_launch_anchor       (_) => true
          case T_1_ary_op(pk: String, _) => true
          case T_n_ary_op(pk: String, _) => if (kind==";" && !parentIsSpaceSeq) {isSpaceSeq = true; false}
                                            else subScriptInfixOpPrecedence(kind) <= subScriptInfixOpPrecedence(pk)
          case _                         => false
        }
        val s = children.map(hierarchyString(_, thisNode, isSpaceSeq)).mkString(if (isSpaceSeq) " " else thisNode.kindAsString)
        if (doParenthesize) "(" + s + ")" else s
      case T_1_ary_op(kind: String, _) =>   kind + hierarchyString(children.head, thisNode, false)
      case T_launch          (_) => thisNode.kindAsString + hierarchyString(children.head, thisNode, false)
      case T_launch_anchor   (_) => thisNode.kindAsString + hierarchyString(children.head, thisNode, false)
      case T_if            (_,_) => "if()["      + hierarchyString(children.head, thisNode, false) + "]"
      case T_if_else     (_,_,_) => "if()["      + hierarchyString(children.head, thisNode, false) + "]else[" + hierarchyString(children.tail.head, thisNode, false) + "]"
      case T_then          (_,_) =>     "["      + hierarchyString(children.head, thisNode, false) + "]then[" + hierarchyString(children.tail.head, thisNode, false) + "]"
      case T_then_else   (_,_,_) =>     "["      + hierarchyString(children.head, thisNode, false) + "]then[" + hierarchyString(children.tail.head, thisNode, false) + 
                                                                                                "]else[" + hierarchyString(children.tail.tail.head, thisNode, false) + "]"
      case T_annotation    (_,_) => thisNode.kindAsString + hierarchyString(children.head, thisNode, false)
      case T_script (_, kind: String, name: Symbol, _) => name.toString + " = " + hierarchyString(children.head, thisNode, false)
    //case T_commscript(_, kind: String, _)                     => "cscript"
    //case T_communication(_, kind: String, names: Seq[Symbol]) => "comm"
      case _ => thisNode.kindAsString
    }
  }
}

trait TemplateNode {
  type N <: CallGraphNodeTrait
  def kindAsString: String = TemplateNode.kindAsString(this)

  /* reconstruct a human readable string representation of the script expression */
  def hierarchyString: String = TemplateNode.hierarchyString(this, null, false)
  
  def owner: AnyRef
  def root:TemplateNode
  def parent:TemplateNode
  var indexAsChild: Int = 0
  var indexInScript: Int = 0
  override def toString = kindAsString
  def children: Seq[TemplateChildNode]
  
  def setIndexes(startIndexInScript: Int, indexForChild: Int): Int = {
    indexAsChild  = indexForChild
    indexInScript = startIndexInScript
    var result    = 1
    var i         = 0
    children.foreach{ c=>
      c.parent = this; 
      result  += c.setIndexes(startIndexInScript + result, i)
      i       += 1
    }
    result
  }
}
trait TemplateCodeHolder[N <: DoCodeHolder[R], R] extends TemplateNode {
  val code: () => N => R
  def execute(here: N): R = code().apply(here)
}

trait TemplateRootNode extends TemplateNode {
  def parent: TemplateNode = null
  def root = this
  setIndexes(0, 0)
}
trait TemplateChildNode extends TemplateNode {
  def root  = {
    if (parent==null) {
      println(this)
    }
    parent.root
  }
  def owner = parent.owner
  var parent: TemplateNode = null
}
trait TemplateNode_0_Trait extends TemplateNode         {                               override val children: Seq[TemplateChildNode] = Nil}
trait TemplateNode_1_Trait extends TemplateNode_0_Trait {def child0: TemplateChildNode; override val children: Seq[TemplateChildNode] = child0::Nil}
trait TemplateNode_2_Trait extends TemplateNode_1_Trait {def child1: TemplateChildNode; override val children: Seq[TemplateChildNode] = child0::child1::Nil}
trait TemplateNode_3_Trait extends TemplateNode_2_Trait {def child2: TemplateChildNode; override val children: Seq[TemplateChildNode] = child0::child1::child2::Nil}
trait TemplateNode_n_Trait extends TemplateNode

trait T_0_ary extends TemplateNode_0_Trait with TemplateChildNode
trait T_1_ary extends TemplateNode_1_Trait with TemplateChildNode
trait T_2_ary extends TemplateNode_2_Trait with TemplateChildNode
trait T_3_ary extends TemplateNode_3_Trait with TemplateChildNode
trait T_n_ary extends TemplateNode_n_Trait with TemplateChildNode

trait T_atomic_action extends T_0_ary // with TemplateCodeHolder[Unit]
// all concrete template node case classes 

case class T_code_normal            (code: () => N_code_normal             => Unit) extends T_atomic_action with TemplateCodeHolder[N_code_normal            , Unit] {type N = N_code_normal         }
case class T_code_tiny              (code: () => N_code_tiny               => Unit) extends T_atomic_action with TemplateCodeHolder[N_code_tiny              , Unit] {type N = N_code_tiny           }
case class T_code_threaded          (code: () => N_code_threaded           => Unit) extends T_atomic_action with TemplateCodeHolder[N_code_threaded          , Unit] {type N = N_code_threaded       }
case class T_code_unsure            (code: () => N_code_unsure             => Unit) extends T_atomic_action with TemplateCodeHolder[N_code_unsure            , Unit] {type N = N_code_unsure         }
case class T_code_eventhandling     (code: () => N_code_eventhandling      => Unit) extends T_atomic_action with TemplateCodeHolder[N_code_eventhandling     , Unit] {type N = N_code_eventhandling  }
case class T_code_eventhandling_loop(code: () => N_code_eventhandling_loop => Unit) extends T_atomic_action with TemplateCodeHolder[N_code_eventhandling_loop, Unit] {type N = N_code_eventhandling_loop}

case class T_localvar[V](isVal: Boolean, isLoop: Boolean, localVariable: LocalVariable[V], code: () => N_localvar[V] => V) extends T_0_ary with TemplateCodeHolder[N_localvar[V],V] { type N = N_localvar[V]}
case class T_privatevar(name: Symbol) extends T_0_ary                    {type N = N_privatevar         }
case class T_while(code: () => N_while => Boolean) extends T_0_ary with TemplateCodeHolder[N_while, Boolean] {type N = N_while              }
case class T_break     ()           extends T_0_ary                                  {type N = N_break              }
case class T_optional_break()       extends T_0_ary                                  {type N = N_optional_break     }
case class T_optional_break_loop()  extends T_0_ary                                  {type N = N_optional_break_loop}
case class T_delta()                extends T_0_ary                                  {type N = N_delta              }
case class T_epsilon()              extends T_0_ary                                  {type N = N_epsilon            }
case class T_nu()                   extends T_0_ary                                  {type N = N_nu                 }
case class T_loop()                 extends T_0_ary                                  {type N = N_loop               }
case class T_if            (code: () => N_if      => Boolean, child0: TemplateChildNode)                            extends T_1_ary with TemplateCodeHolder[N_if     , Boolean] {type N = N_if                 }
case class T_if_else       (code: () => N_if_else => Boolean, child0: TemplateChildNode, child1: TemplateChildNode) extends T_1_ary with TemplateCodeHolder[N_if_else, Boolean] {type N = N_if_else            }
case class T_launch        (child0: TemplateChildNode)                                                              extends T_1_ary                            {type N = N_launch             }
case class T_launch_anchor (child0: TemplateChildNode)                       extends T_1_ary {type N = N_launch_anchor; override def root=this;}
case class T_then     (child0: TemplateChildNode, child1: TemplateChildNode) extends T_1_ary {type N = N_then     }
case class T_then_else(child0: TemplateChildNode, child1: TemplateChildNode,
                                                  child2: TemplateChildNode) extends T_1_ary {type N = N_then_else}

//case class T_0_ary_op(kind: String                                           ) extends T_0_ary  {type N = N_0_ary_op}
case class T_1_ary_op(kind: String,                child0: TemplateChildNode ) extends T_1_ary {type N = N_1_ary_op; override def kindAsString=kind}
case class T_n_ary_op(kind: String, override val children: TemplateChildNode*) extends T_n_ary {type N = N_n_ary_op; override def kindAsString=kind}

case class T_annotation[CN<:CallGraphNodeTrait,CT<:TemplateChildNode](code: () => N_annotation[CN,CT] => Unit, child0: TemplateChildNode) extends T_1_ary with TemplateCodeHolder[N_annotation[CN,CT], Unit] {
  type N=N_annotation[CN,CT] 
}

case class T_call(calleeName: String, code: () => N_call => _scriptType) extends T_0_ary with TemplateCodeHolder[N_call, _scriptType] {type N = N_call}

case class T_script (owner: AnyRef, kind: String, name: Symbol, child0: TemplateChildNode) extends TemplateNode_1_Trait with TemplateRootNode {
  type N = N_script
  override def toString = name.name
}
case class T_commscript(owner: AnyRef, kind: String, communicator: Communicator) extends TemplateNode_0_Trait with TemplateRootNode {
  type N = N_communication
  override def toString = super.toString+" "+communicator.name.name
}
case class T_communication(owner: AnyRef, kind: String, names: Seq[Symbol]) extends TemplateNode_0_Trait with TemplateRootNode {
  type N = N_communication
  override def toString = super.toString+" "+names.mkString(",")
}


case class T_local_valueCode[V<:Any] (kind: String, localVariable: LocalVariable[V], code: () => N_localvar[V]=>V) extends T_0_ary with TemplateCodeHolder[N_localvar[V], V] {type N = N_localvar[V]}

//case class T_0_ary_code (kind: String, code: () => N => Unit)                                                extends TemplateChildNode_0_WithCode[N, Unit]
//case class T_1_ary_code (kind: String, code: () => N => Unit, child0: TemplateChildNode)                          extends TemplateChildNode_1_WithCode[N, Unit]
//case class T_2_ary_code (kind: String, code: () => N => Unit, child0: TemplateChildNode, child1: TemplateChildNode)    extends TemplateChildNode_2_WithCode[N, Unit]  {child1.indexAsChild = 1}

//case class T_0_ary_test[N<:CallGraphNodeTrait[TemplateNode]] (kind: String, code: () => N => Boolean)                                             extends TemplateChildNode_0_WithCode[N, Boolean]
//case class T_1_ary_test[N<:CallGraphNodeTrait[TemplateNode]] (kind: String, code: () => N => Boolean, child0: TemplateChildNode)                       extends TemplateChildNode_1_WithCode[N, Boolean]
//case class T_2_ary_test[N<:CallGraphNodeTrait[TemplateNode]] (kind: String, code: () => N => Boolean, child0: TemplateChildNode, child1: TemplateChildNode) extends TemplateChildNode_2_WithCode[N, Boolean]  {child1.indexAsChild = 1}

// TBD: case class T_match    (code: ScriptNode => Unit, caseParts)    extends TemplateNode;
// TBD: case class T_exception(?)    extends TemplateNode;
// TBD: case class T_for...    extends TemplateNode;

// some utility objects:

object LogicalKind extends Enumeration {
  type LogicalKindType = Value
  val And, Or, None = Value
}

object ExclusiveKind extends Enumeration {
  type ExclusiveKindType = Value
  val All, LeftOnly, None, Disambiguating_all, Disambiguating_leftOnly = Value
}

object T_n_ary_op {
  
  def getLogicalKind(t: T_n_ary_op): LogicalKind.LogicalKindType = getLogicalKind(t.kind)
  def getLogicalKind(kind: String): LogicalKind.LogicalKindType = {
    kind match {
      case ";" | "|;" | "||;" | "|;|" 
         | "&&" | "&" | "&&:" | "&:"
         | "=="  | "==>"  | "&==>"  | "&&==>"
         | "==:" | "==>:" | "&==>:" | "&&==>:"
         | "#" | "#/"          => LogicalKind.And
                             
      case "||"  | "|"    | "|==>"  | "||==>"
         | "||:" | "|:"   | "|==>:" | "||==>:"
         | "|+"  | "|/" 
         | "||+" | "||/" 
         | "|+|" | "|/|" 
         | "+"   | "/" | "%" 
         | "#%"  | "#%#"       => LogicalKind.Or                         
      
      case "#/#/"               => LogicalKind.None
      
      case _ => null
    }
  }
  def getExclusiveKind(t: T_n_ary_op): ExclusiveKind.ExclusiveKindType = getExclusiveKind(t.kind)
  def getExclusiveKind(kind: String): ExclusiveKind.ExclusiveKindType = {
    kind match {
      case ";" | "." | "+"  => ExclusiveKind.All                             
      case "/"              => ExclusiveKind.LeftOnly                             
      case "|;|" | "|+|"    => ExclusiveKind.Disambiguating_all
      case "|/|"            => ExclusiveKind.Disambiguating_leftOnly
      case _                => ExclusiveKind.None
    }
  }
  def isMerge(t: T_n_ary_op): Boolean = isMerge(t.kind)
  def isMerge(kind: String): Boolean = {
    kind match {
      case "&&" | "&" | "&&:" | "&:"
         | "=="  | "==>" | "&==>"  | "&&==>"  | "|==>"  | "||==>"

         | "||"  | "|"  
         | "||:" | "|:"   => true                         
      
      case _ => false
    }
  }
  def isLeftMerge(t: T_n_ary_op): Boolean = isLeftMerge(t.kind)
  def isLeftMerge(kind: String): Boolean = {
    kind match {
      case "&&:" | "&:"
         | "==:"  | "==>:"  | "&==>:"  | "&&==>:"  | "|==>:"  | "||==>:"   
         | "||:" | "|:" => true
      case _            => false
    }
  }
  def isSuspending(t: T_n_ary_op): Boolean = isSuspending(t.kind)
  def isSuspending(kind: String): Boolean = {
    kind match {
      case "#" 
         | "#%" | "#%#"
         | "#/" | "#/#/" => true
      case _             => false
    }
  }
}