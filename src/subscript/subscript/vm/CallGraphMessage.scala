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

  trait CallGraphMessage[N <: CallGraphNodeTrait] {
      var priority = 0 // TBD: determine good priority levels
      var index = -1
	  def node: N

	  val className = "%14s".format(getClass.getSimpleName)
      override def toString = index+" "+className+" "+node
  }
  // various kinds of messages sent around in the script call graph
  abstract class CallGraphMessageN extends CallGraphMessage[CallGraphNodeTrait]
  
	case class Activation   (node: CallGraphNodeTrait) extends CallGraphMessageN {priority = 8 }
	case class Continuation (node: CallGraphTreeNode_n_ary) extends CallGraphMessageN {
	  priority = 4
	  var activation: Activation = null
	  var deactivations: List[Deactivation] = Nil
	  var success: Success = null
	  var break: Break = null
	  var aaActivated: AAActivated = null
	  var caActivated: CAActivated = null
	  var aaStarteds : List[AAStarted] = Nil
	  var aaEndeds   : List[AAEnded  ] = Nil
	  var childNode  : CallGraphNodeTrait = null
	  
	  override def toString = {
	    var result = super.toString
	    if (activation   !=null) result += " "+activation
	    if (deactivations!=Nil ) result += " "+deactivations.mkString
	    if (success      !=null) result += " "+success
	    if (break        !=null) result += " "+break
	    if (aaActivated  !=null) result += " "+aaActivated
	    if (caActivated  !=null) result += " "+caActivated
	    if (aaStarteds   !=Nil ) result += " "+aaStarteds.mkString
	    if (aaEndeds     !=Nil ) result += " "+aaEndeds.mkString
	    if (childNode    !=null) result += " "+childNode
	    result
	  }
	}
	case class Continuation1      (node: N_1_ary_op) extends CallGraphMessageN {priority = 5}
	case class Deactivation       (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait, excluded: Boolean) extends CallGraphMessageN {priority = 6}
	case class Suspend            (node: CallGraphNodeTrait) extends CallGraphMessageN {priority = 9}
	case class Resume             (node: CallGraphNodeTrait) extends CallGraphMessageN {priority = 10}
	case class Exclude          (parent: CallGraphNodeTrait, 
	                               node: CallGraphNodeTrait) extends CallGraphMessageN {priority = 11}
	case class Success            (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait = null) extends CallGraphMessageN {priority = 12}
	case class Break              (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait, activationMode: ActivationMode.ActivationModeType) extends CallGraphMessageN {priority = 13}
	case class AAActivated        (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait) extends CallGraphMessageN {priority = 14}
	case class CAActivated        (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait) extends CallGraphMessageN {priority = 15} // for immediate handling
	case class CAActivatedTBD     (node: N_call            ) extends CallGraphMessageN {priority = 2} // for late handling
	case class AAStarted          (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait) extends CallGraphMessageN {priority = 17}
	case class AAEnded            (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait) extends CallGraphMessageN {priority = 16}
	case class AAToBeExecuted     (node: CallGraphNodeTrait) extends CallGraphMessage[CallGraphNodeTrait] {priority = 1}
	case class AAToBeReexecuted   (node: CallGraphNodeTrait) extends CallGraphMessage[CallGraphNodeTrait] {priority = 0}
	case class AAExecutionFinished(node: CallGraphNodeTrait) extends CallGraphMessage[CallGraphNodeTrait] {priority = 6}
	case object CommunicationMatchingMessage extends CallGraphMessage[CallGraphNodeTrait] {
	  priority = 3 
	  def node:CallGraphNodeTrait = null
	  def activatedCommunicatorCalls = scala.collection.mutable.ArrayBuffer.empty[N_call]
	}
	// TBD: AAActivated etc to inherit from 1 trait; params: 1 node, many children
	// adjust insert method
	// CommunicationMatching should have Set[CommunicationRelation] (?), and have List[Communicators]
	// timestamp of Communicators should be determined by timestamp of newest N_call node
	//
	//
	// Prolog/Linda style question: 
	// can we test all possible communications of a certain type?
	// i.e. 
	// ->..?p:Int? loops and matches all sent integers like <-*1  <-*3
	
