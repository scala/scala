/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

/* General Balanced Trees - highly efficient functional dictionaries.
**
** This is a scala version of gb_trees.erl which is
** copyrighted (C) 1999-2001 by Sven-Olof Nystrom, and Richard Carlsson
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
** ---------------------------------------------------------------------
*/

package scala.collection.immutable;


/** General Balanced Trees - highly efficient functional dictionaries.
 *
 *  An efficient implementation of Prof. Arne Andersson's General
 *  Balanced Trees. These have no storage overhead compared to plain
 *  unbalanced binary trees, and their performance is in general better
 *  than AVL trees.
 *  <p/>
 *  This implementation does not balance the trees after deletions.
 *  Since deletions
 *  don't increase the height of a tree, this should be OK in most
 *  applications. A balance method is provided for those cases
 *  where rebalancing is needed.
 *  <p/>
 *  The tree consists of entries conatining a key with an order.
 *  Concrete implementations of the tree class has to suply the
 *  function entryKey that given an entry in the tree return
 *  its key.
 *  <p/>
 *  When instanciating the tree an order for the keys has to be
 *  supplied.
 *
 *  @author  Erik Stenman
 *  @version 1.0, 10/07/2003
 */
abstract class Tree[A <% Ordered[A], B] with java.io.Serializable {
  /* Data structure:
  ** - size:Int - the number of elements in the tree.
  ** - tree:T, which is composed of nodes of the form:
  **   - GBNode(entry:B, smaller:T, bigger:T), and the "empty tree" node:
  **   - GBNil.
  **
  ** Original balance condition h(T) <= ceil(c * log(|T|)) has been
  ** changed to the similar (but not quite equivalent) condition
  ** 2 ^ h(T) <= |T| ^ c.
  **
  */

  /** The type returned when creating a new tree.
  *   This type should be defined by concrete implementations
  *   e.g. <pre>
  *   class C[T](...) extends Tree[A,B](...) {
  *     type This = C[T];
  *   </pre>
  */
  type This <: Tree[A,B];

  /**
  *  The type of nodes that the tree is build from.
  */
  protected type aNode = GBTree[A,B];

  /** The nodes in the tree.
  */
  protected val tree:aNode = GBNil;

  /** This abstract method should be defined by a concrete implementation
  **   C[T] as something like:
  **    <pre>
  **     override def New(sz:Int,t:aNode):This {
  **       new C[T](order) {
  **        override def size=sz;
  **        override protected val tree:aNode=t;
  **     }
  **    </pre>
  **   The concrete implementation should also override the def of This
  **   <code>override type This = C[T];</code>
  **
  */
  protected def New(sz:Int,t:aNode):This;

  /** The size of the tree, returns 0 (zero) if the tree is empty.
  **  @Returns The number of nodes in the tree as an integer.
  **/
  def size = 0;

  /** Returns the key of an entry.
  *   This method has to be defined by concrete implementations
  *   of the class.
  */
  def entryKey(entry:B):A;


  /** Is the given key mapped to a value by this map?
  *
  *  @param   key        the key
  *  @return true, iff there is a mapping for key in this map
  */
  def is_defined(key:A) = tree.is_defined(key);


  /**
  *   A new tree with the entry added is returned,
  *   assuming that key is <em>not</em> in the tree.
  */
  def add(key:A,entry:B):This = {
    val newSize = size+1;
    New(newSize,
        tree.insert(key,
                    entry,
                    pow(newSize, p)).node);
  }

  /**
  *   A new tree with the entry added is returned,
  *   if key is <em>not</em> in the tree, otherwise
  *   the key is updated with the new entry.
  */
  def update_or_add(key:A, entry:B):This = {
    if(is_defined(key)) New(size,tree.update(key,entry))
    else add(key,entry);
  }


  /** Removes the key from the tree.
  */
  def delete_any(key:A) =
    if(is_defined(key)) delete(key) else
      // TODO: Avoid this creation by convincing the type system that this has type This.
      New(size,tree);

  /** Removes the key from the tree, assumimg that key is present.
  */
  private def delete(key:A) = New(size - 1, tree.delete(key));

  /** Check if this map maps <code>key</code> to a value and return the
  *  value if it exists.
  *
  *  @param  key     the key of the mapping of interest
  *  @return the value of the mapping, if it exists
  */
  def findValue(key:A) =  tree.get(key);

  /**
  *  Gives you an iterator over all elements in the tree.
  *  The iterator structure corresponds to
  *  the call stack of an in-order traversal.
  *
  * Note: The iterator itself has a state, i.e., it is not functional.
  */
  def entries:Iterator[B] =
    new Iterator[B] {
      var iter = tree.mk_iter(scala.Nil);
      def hasNext = !iter.isEmpty;
      def next =
        iter.match {
          case (GBNode(v,_,t)::iter_tail) => {
            iter= t.mk_iter(iter_tail);
            v;
          }
          case scala.Nil =>
            error("next on empty iterator");
        }
    }

  /** Create a new balanced tree from the tree.
  *  Might be useful to call after many deletions,
  *  since deletion does not rebalance the tree.
  **/
  def balance = New(size,tree.balance(size));


  case object GBNil extends GBTree[A,B] {
    def count = Info(1,0);
    def is_defined(key:A) = false;
    def get(_key:A) = None;
    def apply(key:A) = error("key " + key + " not found");
    def update(key:A, value:B) = error("key " + key + " not found");
    def insert(key:A, value:B, s:int):anInsertTree =
      if (s == 0) INode(GBNode(value, GBNil, GBNil), 1, 1);
      else ITree(GBNode(value, GBNil, GBNil));
    def toList(acc:List[B]) = acc;
    def mk_iter(iter_tail:List[GBTree[A,B]]) = iter_tail;
    def merge(larger:GBTree[A,B]) = larger;
    def take_smallest:Pair[B,GBTree[A,B]] = error("Take Smallest on empty tree");
    def delete(_key:A) = error("Delete on empty tree.");
    def balance(s:int) = this;
    override def hashCode() = 0;
  }

  private case class GBNode(value:B,smaller:GBTree[A,B],bigger:GBTree[A,B]) extends GBTree[A,B] {
    def count:Info = {
      if (smaller == GBNil && bigger == GBNil) Info(1,1);
      else {
        val sInfo = smaller.count;
        val bInfo = bigger.count;
        Info(mul2(max(sInfo.height,bInfo.height)),
             sInfo.size + bInfo.size +1);
      }
    }

    def is_defined(key:A):Boolean = {
      if(key < entryKey(value)) smaller.is_defined(key)
      else if(key > entryKey(value)) bigger.is_defined(key)
      else true;
    }

    def get(key:A):Option[B] =
      if (key < entryKey(value)) smaller.get(key);
      else if (key > entryKey(value)) bigger.get(key);
        else Some(value);

    def apply(key:A):B =
      if (key < entryKey(value)) smaller.apply(key);
        else if (key > entryKey(value)) bigger.apply(key);
        else value;

    def update(key:A, newValue:B):aNode =
      if (key < entryKey(value))
        GBNode(value, smaller.update(key,newValue), bigger);
      else if (key > entryKey(value))
        GBNode(value, smaller, bigger.update(key,newValue));
      else
        GBNode(newValue, smaller, bigger);

    def insert(key:A, newValue:B, s:int):anInsertTree = {
      if(key < entryKey(value))
        smaller.insert(key, newValue, div2(s)).insert_left(value,bigger);
      else if(key > entryKey(value))
        bigger.insert(key, newValue, div2(s)).insert_right(value,smaller);
      else error("Key exists" + key);
    }

    def toList(acc:List[B]):List[B] =
      smaller.toList(value::bigger.toList(acc));

    def mk_iter(iter_tail:List[aNode]):List[aNode] =
      if (smaller == GBNil) this::iter_tail;
      else smaller.mk_iter(this::iter_tail);


    def delete(key:A):aNode = {
      if (key < entryKey(value))
        GBNode(value, smaller.delete(key), bigger);
      else
        if (key > entryKey(value))
          GBNode(value, smaller, bigger.delete(key));
        else
          smaller.merge(bigger)
    }

    def merge(larger:aNode) = {
      if(larger==GBNil) this;
      else {
        val Pair(value1,larger1) = larger.take_smallest;
        GBNode(value1,this,larger1);
      }
    }

    def take_smallest:Pair[B,aNode] = {
      if (smaller == GBNil) Pair(value,bigger);
      else {
        val Pair(value1, smaller1) = smaller.take_smallest;
        Pair(value1, GBNode(value, smaller1, bigger));
      }
    }

    def balance(s:int):GBTree[A,B] = balance_list(toList(scala.Nil), s);

    override def hashCode() =
      value.hashCode() + smaller.hashCode() + bigger.hashCode();

  }


  protected def balance_list(list:List[B], s:int) = balance_list_1(list, s)._1;

  protected def balance_list_1(list:List[B], s:int):Pair[aNode,List[B]] = {
    if(s > 1) {
      val sm = s - 1;
      val s2 = div2(sm);
      val s1 = sm - s2;
      val Pair(t1,v::l1) = balance_list_1(list, s1);
      val Pair(t2, l2) = balance_list_1(l1, s2);
      val t = GBNode(v, t1, t2);
      Pair(t, l2);
    } else
      if(s == 1) {
        val v = list.head;
        Pair(GBNode(v,GBNil,GBNil), list.tail);
      } else
        Pair(GBNil,list);
  }



  // %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

  protected case class ITree(t:aNode) extends InsertTree[A,B] {
    def insert_left(value:B,bigger:aNode) = ITree(GBNode(value,t,bigger));
    def insert_right(value:B,smaller:aNode) = ITree(GBNode(value,smaller,t));
    def node = t;
  }
  protected case class INode(t1:aNode,height:int,siz:int) extends InsertTree[A,B]{
    def insert_left(value:B,bigger:aNode) = balance_p(GBNode(value, t1, bigger), bigger);
    def insert_right(value:B,smaller:aNode) = balance_p(GBNode(value, smaller, t1),smaller);
    protected def balance_p(t:aNode,subtree:aNode):anInsertTree = {
      val info = subtree.count;
      val totalHeight = mul2(max(height, info.height));
      val totalSize = siz + info.size + 1;
      val BalanceHeight = pow(totalSize, p);
      if(totalHeight > BalanceHeight) ITree(t.balance(totalSize));
      else INode(t, totalHeight, totalSize);
    }
    def node = t1;
  }

  /* -----------------------------------------------------------
  // Some helpful definitions.
  */
  private val p = 2; // It seems that p = 2 is optimal for sorted keys */
  private def pow(a:int, b:int):int =
    b.match {
      case 2 => a * a;
      case 1 => a;
      case x if x > 0 => a * pow(a, b-1);
    };
  private def div2(x:int) = x >> 1;
  private def mul2(x:int) = x << 1;
  private def max(x:int, y:int):int = if(x>y) x else y;


} // End of class Tree...

private abstract class InsertTree[A,B]() {
  type aNode = GBTree[A,B];
  type anInsertTree = InsertTree[A,B];

  def insert_left(v:B,t:aNode):anInsertTree;
  def insert_right(v:B,t:aNode):anInsertTree;
  def node:aNode;
}

/**
*  GBTree is an internal class used by Tree.
*/

private abstract class GBTree[A,B] {
  type aNode = GBTree[A,B];
  type anInsertTree = InsertTree[A,B];

  /** Calculates 2^h, and size, where h is the height of the tree
  *   and size is the number of nodes in the tree.
  */
  def count:Info;
  def is_defined(Key:A):Boolean;
  def get(key:A):Option[B];
  def apply(key:A):B;
  def update(key:A, value:B):aNode;
  def insert(key:A, value:B, size:int):anInsertTree;
  def toList(acc:List[B]):List[B];
  def mk_iter(iter_tail:List[aNode]):List[aNode];
  def delete(key:A):aNode;
  def merge(t:aNode):aNode;
  def take_smallest:Pair[B,aNode];
  def balance(s:int):GBTree[A,B];

  case class Info(height:int, size:int);


}




