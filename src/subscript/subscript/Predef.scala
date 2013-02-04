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

import subscript.vm._
import subscript.DSL._

// Predefined stuff - pass and some scripts: times, delta, epsilon, nu
//
object Predef {
  def pass    (implicit node: CallGraphTreeNode[_]): Int = node.pass
  def pass_up1(implicit node: CallGraphTreeNode[_]): Int = node.n_ary_op_ancestor.pass
  def pass_up2(implicit node: CallGraphTreeNode[_]): Int = node.n_ary_op_ancestor.n_ary_op_ancestor.pass
  
//  scripts
//    times(n:Int) = while(pass<n)
//    delta        = (-)
//    epsilon      = (+)
//    nu           = (+-)

  def _times(_n: FormalInputParameter[Int]) = _script(this, 'times, _n~'n) {_while{implicit here=>pass<_n.value}}
}