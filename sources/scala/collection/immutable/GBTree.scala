/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

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

object GBTree {
    /** An empty GBTree. */
    def Empty[A <: Ord[A], B] = new GBTree[A, B];
}

/**
 * General Balanced Trees - highly efficient functional dictionaries.
 *
 * An efficient implementation of Prof. Arne Andersson's General
 * Balanced Trees. These have no storage overhead compared to plain
 * unbalanced binary trees, and their performance is in general better
 * than AVL trees.
 * <p>
 * I make no attempt to balance trees after deletions. Since deletions
 * don't increase the height of a tree, I figure this is OK.
 *
 *  @author  Erik Stenman
 *  @version 1.0, 10/07/2003
 */

/* Data structure:
** - Size - the number of elements in the tree.
** - Tree, which is composed of nodes of the form:
**   - Node(Key, Value, Smaller, Bigger), and the "empty tree" node:
**   - Nil().
**
** Original balance condition h(T) <= ceil(c * log(|T|)) has been
** changed to the similar (but not quite equivalent) condition 2 ^ h(T)
** <= |T| ^ c. I figure this should also be OK.
**
*/

class GBTree[A <: Ord[A], B]() with scala.collection.immutable.Map[A, B, GBTree[A,B]] {
  private type TREE = Tree[A,B];

  /**
   * Returns the number of nodes in the tree as an integer.
   *
   * @return   Returns 0 (zero) if the tree is empty.
   */
  def size = 0;

  protected val tree:TREE = Nil[A,B]();

  /* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   * Factory method to create new GB-trees.
   */
  private def mkGBTree[C >: A <: Ord[C],D>:B](sz:int,t:Tree[C,D]) =
      new GBTree[A,B](){
	  override def size=sz;
	  override protected val tree:this.Tree[A,B]=t.asInstanceOf[this.Tree[A,B]];
      };


  /* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */


  /**
   * Checks if the tree is empty.
   *
   * @return true, iff there is no element in the tree.
   */
  override def isEmpty = size == 0;

  /**
   * Looks up the key in the tree;
   *
   * @return Option.Some(v),
   *         or Option.None if the key is not present.
   */
  def get(key:A) = get_1(key, tree);

  /* Ord is an arithmetic total order, so we should not
  ** test exact equality for the keys. (If we do, then it becomes
  ** possible that neither `>', `<', nor `=:=' matches.) Testing '<'
  ** and '>' first is statistically better than testing for
  ** equality, and also allows us to skip the test completely in the
  ** remaining case.
  */
  private def get_1(key:A,t:Tree[A,B]):Option[B] =
    t.match {
      case Node(key1,_,smaller,_) if (key < key1) =>
      get_1(key, smaller);
      case Node(key1, _, _, bigger) if (key > key1) =>
      get_1(key, bigger);
      case Node(_, value, _, _) => Some(value);
      case Nil() => None;
    }

  /**
   * This is a specialized version of `get'.
   */
  override def contains(key:A) = is_defined_1(key, tree);

  private def is_defined_1(key:A, t:Tree[A,B]):Boolean =
    t.match {
      case Node(key1, _, smaller, _) if(key < key1) =>
      is_defined_1(key, smaller);
      case Node(key1, _, _, bigger) if(key > key1) =>
      is_defined_1(key, bigger);
      case Node(_, _, _, _) => true;
      case Nil() => false
    }

    /**
     * Retrieves the value stored with the <code>key</code>
     * in the tree. Assumes that the key is present in the tree.
     *
     * @return the value stored with the <code>key</code>.
     * @throws "key not found".
     */
    override def apply(key:A):B = apply_1(key, tree);

    private def apply_1(key:A,t:Tree[A,B]):B =
	t.match {
	    case Node(key1, _, smaller, _) if(key < key1) =>
	      apply_1(key, smaller);
	    case Node(key1, _, _, bigger) if(key > key1) =>
	      apply_1(key, bigger);
	    case Node(_, value, _, _) => value;
	    case Nil() => error("key not found")
	};

    def update(key:A, value:B):GBTree[A,B] =
	if(contains(key)) update_1(key, value)
			      else insert(key, value);

    private def update_1(key:A,  value:B):GBTree[A,B] = {
	val t1:Tree[A,B] = update_1(key, value, tree);
	mkGBTree(size,t1);
    }
    /** See `get' for notes on the term comparison order.
     **/
    private def update_1(key:A, value:B, t:Tree[A,B]):Tree[A,B] =
	t.match{
	    case Node(key1, v, smaller, bigger) if(key < key1) =>
	    Node(key1, v, update_1(key, value, smaller), bigger);
	    case Node(key1, v, smaller, bigger) if(key > key1) =>
	    Node(key1, v, smaller, update_1(key, value, bigger));
	    case Node(_, _, smaller, bigger) =>
	    Node(key, value, smaller, bigger);
	};

    // %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
  abstract class InsertTree[C <: Ord[C],D]();
  case class ITree[C <: Ord[C],D](t:Tree[C,D]) extends InsertTree[C,D];
  case class INode[C,D](t:Tree[A,B],height:int,size:int) extends InsertTree[A,B];



    /**
     * Inserts the key <code>key</code>
     * with value <code>value</code> into the tree; returns
     * the new tree.
     * Assumes that the key is *not* present in the tree.
     *
     * @return tree:GBTree[A,B].
     * @throws GBTree.KeyExists(key)
     */
    def insert(key:A, value:B):GBTree[A,B] = {
	val s1 = size + 1;

	val ITree(t1) = insert_1(key, value, tree, pow(s1, p));
	mkGBTree(s1,t1);
    }

    private def insert_1(key:A,
			 value:B,
			 t0:Tree[A,B], s:int):InsertTree[A,B] = {
	t0.match {
	    case Node(key1, v, smaller, bigger) if(key < key1) =>
	    insert_1(key, value, smaller, div2(s)).match {
		case ITree(t1) =>
		ITree(Node(key1, v, t1, bigger));
		case INode(t1, h1, s1) =>
		val t = Node(key1, v, t1, bigger);
		val iBigger = bigger.count;
		val h = mul2(max(h1, iBigger.height));
		val ss = s1 + iBigger.size + 1;
		val pp = pow(ss, p);
		if(h > pp) ITree(balance_t(t, ss));
		else INode(t,h,ss);
	    };
	    case Node(key1, v, smaller, bigger) if(key > key1) =>
	    insert_1(key, value, bigger, div2(s)).match {
		case ITree(t1) =>
		ITree(Node(key1, v, smaller, t1));

		case INode(t1, h1, s1) =>
		val t = Node(key1, v, smaller, t1);
		val iSmaller = smaller.count;
		val h = mul2(max(h1, iSmaller.height));
		val ss = s1 + iSmaller.size + 1;
		val pp = pow(ss, p);
		if(h > pp) ITree(balance_t(t, ss));
		else INode(t, h, ss);
	    };
	    case Nil() if(s == 0) =>
	    INode(Node(key, value, Nil(), Nil()), 1, 1);
	    case Nil() =>
	    ITree(Node(key, value, Nil(), Nil()));
	    case _ =>
 	    throw new KeyExists(key);
	}
    }


    private def balance_t(t:Tree[A,B], s:int) =
	balance_list(to_list_1(t), s);


    def balance = mkGBTree(size,balance_t(tree,size));


    private def balance_list(list:List[Pair[A,B]], s:int) = {
	val Pair(t, _) = balance_list_1(list, s);
	t;
    }

    private def balance_list_1(list:List[Pair[A,B]], s:int):
	Pair[Tree[A,B],List[Pair[A,B]]] = {
	    if(s > 1) {
		val sm = s - 1;
		val s2 = div2(sm);
		val s1 = sm - s2;
		val Pair(t1,Pair(k, v)::l1) = balance_list_1(list, s1);
		val Pair(t2, l2) = balance_list_1(l1, s2);
		val t = Node(k, v, t1, t2);
		Pair(t, l2);
	    } else
	    if(s == 1) {
		val Pair(key,v) = list.head;
		Pair(Node(key,v,Nil(),Nil()), list.tail);
	    } else
	    Pair(Nil(),list);
	}


    private def to_list_1(t:Tree[A,B]) = to_list(t, scala.Nil);

    private def to_list(t:Tree[A,B], list:List[Pair[A,B]]):List[Pair[A,B]] = {
	t.match {
	    case Node(key, value, small, big) =>
	    to_list(small, Pair(key, value)::to_list(big, list));
	    case Nil() => list;
	}
    }



    override def toList =
	to_list(tree, scala.Nil);

    def -(key:A):GBTree[A,B] = delete_any(key);

    // The iterator structure is really just a list corresponding to
    // the call stack of an in-order traversal. This is quite fast.
    //
    // Note: The iterator has a state, i.e., it is not functional.
    def elements:Iterator[Pair[A,B]] =
	new Iterator[Pair[A,B]] {
	    var iter = mk_iter(tree,scala.Nil);
	    def hasNext = !iter.isEmpty;
	    def next =
	    iter.match {
		case (Node(x,v,_,t)::iter_tail) =>
		iter= iter_tail;
		Pair(x,v);
		case scala.Nil =>
		error("next on empty iterator");
	    }
	}


    private def mk_iter(t:TREE,iter_tail:List[TREE]):List[TREE] = {
	t.match {
	    case Node(_,_,Nil(),_) => t::iter_tail;
	    case Node(_,_,l,_) => mk_iter(l,t::iter_tail);
	    case Nil() => iter_tail;
	}
    }


    def delete_any(key:A):GBTree[A,B] =
	if(contains(key)) delete(key)
				else this;


    // delete. Assumes that key is present.

    def delete(key:A):GBTree[A,B] =
	mkGBTree(size - 1, delete_1(key,tree));

    // See `get' for notes on the term comparison order.
    def delete_1(key:A,t:TREE):TREE = {
	t.match {
	    case Node(key1, value, smaller, larger) if key < key1 =>
	    val smaller1 = delete_1(key, smaller);
	    Node(key1, value, smaller1, larger);
	    case Node(key1, value, smaller, bigger) if key > key1 =>
	    val bigger1 = delete_1(key, bigger);
	    Node(key1, value, smaller, bigger1);
	    case Node(_,_,smaller,larger) =>
	    merge(smaller, larger)
	}
    }

    private def merge(smaller:TREE,larger:TREE) = {
	if(larger==Nil()) smaller;
	else
	if(smaller==Nil()) larger;
	else {
	    val Tuple3(key,value,larger1) = take_smallest1(larger);
	    Node(key,value,smaller, larger1);
	}
    }

    private def take_smallest1(t:TREE):Tuple3[A,B,TREE] = {
	t.match {
	    case Node(key, value, Nil(), larger) =>
	    Tuple3(key, value, larger);
	    case Node(key, value, smaller, larger) =>
	    val Tuple3(key1, value1, smaller1) = take_smallest1(smaller);
	    Tuple3(key1, value1, Node(key, value, smaller1, larger));
	}
    }


  abstract class Tree[A <: Ord[A],B]() {
    def count:Info;
  }

  private case class Node(key:A,value:B,smaller:TREE,bigger:TREE)
	extends Tree[A,B] {
	  def count:Info = {
	    if (smaller == Nil() && bigger == Nil())
	      new Info(1,1);
	    else {
	      val sInfo = smaller.count;
	      val bInfo = bigger.count;
	      new Info(mul2(max(sInfo.height,bInfo.height)),
		       sInfo.size + bInfo.size +1);
	    }
	  }
	}

  private case class Nil[A <: Ord[A],B]() extends Tree[A,B] {
    def count = new Info(1,0);
  }

  class KeyExists(key:Any) extends java.lang.Exception();

  private class Info(h:int, s:int) {
    val height = h;
    val size = s;
  }

  /* -----------------------------------------------------------
  // Some helpful definitions.
  */
  private val p = 2; // It seems that p = 2 is optimal for sorted keys */
  private def pow(a:int, b:int):int =
    a.match {
      case 2 => a * a;
      case 1 => a;
      case x if x > 0 => a * pow(a, b-1);
    };
  private def div2(x:int) = x >> 1;
  private def mul2(x:int) = x << 1;
  private def max(x:int, y:int) = if(x < y) y; else x;



}

/*
** - from_orddict(L): turns an ordered list L of Pair(Key, Value) pairs into
**   a tree. The list must not contain duplicate keys.
**
** - smallest: returns Pair(X, V), where X is the smallest key in the tree,
**   and V is the value associated with X in the tree. Assumes that the tree
**   is nonempty.
**
** - largest: returns Pair(X, V), where X is the largest key in tree the tree,
**   and V is the value associated with X in the tree. Assumes that the tree
**   is nonempty.
**
** - take_smallest: returns Tuple3(X, V, T1), where X is the smallest key
**   in the tree, V is the value associated with X in the tree, and T1 is the
**   tree with key X deleted. Assumes that the tree is nonempty.
**
** - take_largest: returns Tuple3(X, V, T1), where X is the largest key
**   in the tree, V is the value associated with X, and T1 is the
**   tree with key X deleted. Assumes that the tree is nonempty.


from_orddict(L) =>
    S = length(L),
    {S, balance_list(L, S)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take_smallest({Size, Tree}) =>
    {Key, Value, Larger} = take_smallest1(Tree),
    {Key, Value, {Size - 1, Larger}}.



smallest({_, Tree}) =>
    smallest_1(Tree).

smallest_1({Key, Value, nil, _Larger}) =>
    {Key, Value};
smallest_1({_Key, _Value, Smaller, _Larger}) =>
    smallest_1(Smaller).

take_largest({Size, Tree}) =>
    {Key, Value, Smaller} = take_largest1(Tree),
    {Key, Value, {Size - 1, Smaller}}.

take_largest1({Key, Value, Smaller, nil}) =>
    {Key, Value, Smaller};
take_largest1({Key, Value, Smaller, Larger}) =>
    {Key1, Value1, Larger1} = take_largest1(Larger),
    {Key1, Value1, {Key, Value, Smaller, Larger1}}.

largest({_, Tree}) =>
    largest_1(Tree).

largest_1({Key, Value, _Smaller, nil}) =>
    {Key, Value};
largest_1({_Key, _Value, _Smaller, Larger}) =>
    largest_1(Larger).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






*/

/*
**
** - update(X, V): updates key X to value V in the tree; returns the
**   new tree. Assumes that the key is present in the tree.
**
** - enter(X, V): inserts key X with value V into the tree if the key
**   is not present in the tree, otherwise updates key X to value V in
**   the tree. Returns the new tree.
**
** - delete(X): removes key X from the tree; returns new tree. Assumes
**   that the key is present in the tree.
**
** - delete_any(X): removes key X from the tree if the key is present
**   in the tree, otherwise does nothing; returns new tree.
**
** - balance: rebalances the tree. Note that this is rarely necessary,
**   but may be motivated when a large number of entries have been
**   deleted from the tree without further insertions. Rebalancing could
**   then be forced in order to minimise lookup times, since deletion
**   only does not rebalance the tree.
**
** - contains(X): returns `true' if key X is present in the tree, and
**   `false' otherwise.
**
** - keys: returns an ordered list of all keys in the tree.
**
** - values: returns the list of values for all keys in the tree,
**   sorted by their corresponding keys. Duplicates are not removed.
**
** - to_list: returns an ordered list of Pair(Key, Value) for all
**   keys in the tree.
**
**
** - iterator: returns an iterator that can be used for traversing
**   the entries of the tree; see `next'. The implementation of this is
**   very efficient; traversing the whole tree using `next' is only
**   slightly slower than getting the list of all elements using
**   `to_list' and traversing that. The main advantage of the iterator
**   approach is that it does not require the complete list of all
**   elements to be built in memory at one time.
**
** - next: returns Tuple3(X, V, S1) where X is the smallest key referred to
**   by the iterator, and S1 is the new iterator to be used for
**   traversing the remaining entries, or the atom `none' if no entries
**   remain.
*/
