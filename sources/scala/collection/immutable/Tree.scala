/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
** =====================================================================*/
/* General Balanced Trees - highly efficient functional dictionaries.
**
** This is a scala version of gb_trees.erl which is
** copyrighted (C) 1999-2001 by Sven-Olof Nyström, and Richard Carlsson
**
** An efficient implementation of Prof. Arne Andersson's General
** Balanced Trees. These have no storage overhead compared to plain
** unbalanced binary trees, and their performance is in general better
** than AVL trees.
** ---------------------------------------------------------------------
** This library is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** This library is distributed in the hope that it will be useful, but
** WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
** Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public
** License along with this library; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
** USA
**
** Author contact: erik.stenman@epfl.ch
**                 (svenolof@csd.uu.se, richardc@csd.uu.se)
** ---------------------------------------------------------------------
*/

package scala.collection.immutable;


/** General Balanced Trees - highly efficient functional dictionaries.
**
** An efficient implementation of Prof. Arne Andersson's General
** Balanced Trees. These have no storage overhead compared to plain
** unbalanced binary trees, and their performance is in general better
** than AVL trees.
** <p>
** This implementation does not balance the trees after deletions.
** Since deletions
** don't increase the height of a tree, this should be OK in most
** applications. A balance method is provided for dose cases
** where rebalancing is needed.
**
**  @author  Erik Stenman
**  @version 1.0, 10/07/2003
*/

class Tree[KEY,Entry](order:Order[KEY],entryKey:Entry=>KEY) {
    /* Data structure:
    ** - Size:int - the number of elements in the tree.
    ** - Tree:T, which is composed of nodes of the form:
    **   - Node(Key, Value, Smaller, Bigger), and the "empty tree" node:
    **   - Nil().
    **
    ** Original balance condition h(T) <= ceil(c * log(|T|)) has been
    ** changed to the similar (but not quite equivalent) condition 2 ^ h(T)
    ** <= |T| ^ c. I figure this should also be OK.
    **
    */

    protected type aNode = Tree[KEY,Entry]#T;
    protected type anInsertTree = Tree[KEY,Entry]#InsertTree;
    protected val tree:aNode = Nil();

    /** The size of the tree, returns 0 (zero) if the tree is empty.
     ** @Returns The number of nodes in the tree as an integer.
     **/
    def size = 0;

    /* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     * Factory method to create new trees.
     */
    private def mkTree(sz:int,t:aNode):Tree[KEY,Entry] =
	new Tree[KEY,Entry](order,entryKey){
	    override def size=sz;
	    override protected val tree:aNode=t;
	};
    /* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */


    /** Create a new balanced tree from the tree.
     *  Might be useful to call after many deletions,
     *  since deletion does not rebalance the tree.
     **/
    def balance = mkTree(size,tree.balance(size));



    // The iterator structure is really just a list corresponding to
    // the call stack of an in-order traversal.
    //
    // Note: The iterator has a state, i.e., it is not functional.
    def entries:Iterator[Entry] =
	new Iterator[Entry] {
	    var iter = tree.mk_iter(scala.Nil);
	    def hasNext = !iter.isEmpty;
	    def next =
	    iter.match {
		case (Node(v,_,t)::iter_tail) =>
		iter= iter_tail;
		v;
		case scala.Nil =>
		error("next on empty iterator");
	    }
	}


  private abstract class T() {
    def count:Info;
    def is_defined(Key:KEY):Boolean;
    def get(key:KEY):Option[Entry];
    def apply(key:KEY):Entry;
    def update(key:KEY, value:Entry):aNode;
    def insert(key:KEY, value:Entry, size:int):anInsertTree;
    def toList(acc:List[Entry]):List[Entry];
    def mk_iter(iter_tail:List[aNode]):List[aNode];
    def delete(key:KEY):aNode;
    def merge(t:aNode):aNode;
    def take_smallest:Pair[Entry,aNode];

    def balance(s:int) = balance_list(toList(scala.Nil), s);

    private def balance_list(list:List[Entry], s:int) = {
	val Pair(t, _) = balance_list_1(list, s);
	t;
    }

    private def balance_list_1(list:List[Entry], s:int):
	Pair[aNode,List[Entry]] = {
	    if(s > 1) {
		val sm = s - 1;
		val s2 = div2(sm);
		val s1 = sm - s2;
		val Pair(t1,v::l1) = balance_list_1(list, s1);
		val Pair(t2, l2) = balance_list_1(l1, s2);
		val t = Node(v, t1, t2);
		Pair(t, l2);
	    } else
	    if(s == 1) {
		val v = list.head;
		Pair(Node(v,Nil(),Nil()), list.tail);
	    } else
	    Pair(Nil(),list);
	}



  }



  private case class Node(value:Entry,smaller:aNode,bigger:aNode)
	extends T {
      def count:Info = {
	  if (smaller == Nil() && bigger == Nil())
	  Info(1,1);
	  else {
	      val sInfo = smaller.count;
	      val bInfo = bigger.count;
	      Info(mul2(max(sInfo.height,bInfo.height)),
		       sInfo.size + bInfo.size +1);
	  }
      }

      def is_defined(key:KEY):Boolean =
	  if(order.lt(key,entryKey(value))) smaller.is_defined(key);
	  else if (order.gt(key,entryKey(value))) bigger.is_defined(key);
	  else true;

      def get(key:KEY):Option[Entry] =
	  if (order.lt(key,entryKey(value))) smaller.get(key);
	  else if (order.gt(key,entryKey(value))) bigger.get(key);
	  else Some(value);

      def apply(key:KEY):Entry =
	  if (order.lt(key,entryKey(value))) smaller.apply(key);
	  else if (order.gt(key,entryKey(value))) bigger.apply(key);
	  else value;

      def update(key:KEY, newValue:Entry):aNode =
	  if (order.lt(key,entryKey(value)))
	      Node(value, smaller.update(key,newValue), bigger);
	  else if (order.gt(key,entryKey(value)))
	      Node(value, smaller, bigger.update(key,newValue));
	  else
	      this;

      def insert(key:KEY, newValue:Entry, s:int):anInsertTree =
	  if(order.lt(key,entryKey(value)))
	      smaller.insert(key, newValue, div2(s)).insert_left(value,bigger);
	  else if(order.gt(key,entryKey(value)))
	      bigger.insert(key, value, div2(s)).insert_right(value,smaller);
	  else error("Key exists" + key);

      def toList(acc:List[Entry]):List[Entry] =
	  smaller.toList(value::bigger.toList(acc));

      def mk_iter(iter_tail:List[aNode]):List[aNode] =
	  if (smaller == Nil()) this::iter_tail;
	  else smaller.mk_iter(this::iter_tail);


      def delete(key:KEY):aNode = {
	  if (order.lt(key,entryKey(value)))
	  Node(value, smaller.delete(key), bigger);
	  else
	  if (order.gt(key,entryKey(value)))
	  Node(value, smaller, bigger.delete(key));
	  else
	  smaller.merge(bigger)
	}

      def merge(larger:aNode) = {
	if(larger==Nil()) this;
	else {
	    val Pair(value,larger1) = larger.take_smallest;
	    Node(value,smaller,larger1);
	}
      }

      def take_smallest:Pair[Entry,aNode] = {
	  if (smaller == Nil) Pair(value,bigger);
	  else {
	      val Pair(value1, smaller1) = smaller.take_smallest;
	      Pair(value1, Node(value, smaller1, bigger));
	  }
      }
  }


    private case class Nil() extends T {
	def count = Info(1,0);
	def is_defined(_key:KEY) = false;
	def get(_key:KEY) = None;
	def apply(key:KEY) = error("key " + key + "not found");
	def update(key:KEY, value:Entry) = error("key " + key + "not found");
	def insert(key:KEY, value:Entry, s:int) =
	    if (s == 0) INode(Node(value, Nil(), Nil()), 1, 1);
	    else ITree(Node(value, Nil(), Nil()));
	def toList(acc:List[Entry]) = acc;
	def mk_iter(iter_tail:List[aNode]) = iter_tail;
	def merge(larger:aNode) = larger;
	def take_smallest:Pair[Entry,aNode] = error("Take Smallest on empty tree");
	def delete(_key:KEY) = error("Delete on empty tree.");
    }


    private case class Info(height:int, size:int) {}


    /* -----------------------------------------------------------
    // Some helpful definitions.
    */
    protected val p = 2; // It seems that p = 2 is optimal for sorted keys */
    protected def pow(a:int, b:int):int =
      b.match {
	    case 2 => a * a;
	    case 1 => a;
            case 0 => 1;
	    case x if x > 0 => a * pow(a, b-1);
	};
    private def div2(x:int) = x >> 1;
    private def mul2(x:int) = x << 1;
    private def max(x:int, y:int) = if(x<y) y; else x;



    // %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
    protected abstract class InsertTree() {
	def insert_left(v:Entry,t:aNode):anInsertTree;
	def insert_right(v:Entry,t:aNode):anInsertTree;
	def node:aNode;
    }
    private case class ITree(t:aNode) extends InsertTree {
	def insert_left(value:Entry,bigger:aNode) =
	    ITree(Node(value,t,bigger));
	def insert_right(value:Entry,smaller:aNode) =
	    ITree(Node(value,smaller,t));
	def node = t;
    }
    private case class INode(t1:aNode,height:int,size:int) extends InsertTree{
	def insert_left(value:Entry,bigger:aNode) = {
	    insert(Node(value, t1, bigger), bigger);
	}
	def insert_right(value:Entry,smaller:aNode) = {
	    insert(Node(value, smaller, t1),smaller);
	}
	private def insert(t:aNode,subtree:aNode):anInsertTree = {
	    val info = subtree.count;
	    val totalHeight = mul2(max(height, info.height));
	    val totalSize = size + info.size + 1;
	    val BalanceHeight = pow(totalSize, p);
	    if(totalHeight > BalanceHeight) ITree(t.balance(totalSize));
	    else INode(t, totalHeight, totalSize);
	}
	def node = t1;
    }

}

