/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                  */

package scala.concurrent;

/**
* Library for using join-calculus concurrent primitives in Scala.
*/
object jolib {

  /////////////////// QUEUE /////////////////////////

  class Queue[a] {
    private var queue: List[a] = Nil;
    def enqueue(x: a) = queue = queue ::: List(x);
    def dequeue: a = { val x = queue.head; queue = queue.tail; x };
    def front: a = queue.head;
    def isEmpty = queue.isEmpty;
  }

  /////////////////// JOIN /////////////////////////

  type Pattern = List[Signal];

  type Rule = PartialFunction[List[Any], unit];

  class Join with Monitor {

    private var ruls: List[Pair[Pattern, Rule]] = null;

    def canMatch(p: Pattern) =
      p forall { s => !s.queue.isEmpty };

    def values(p: Pattern) =
      p map { s => s.queue.dequeue };

    def rules(rs: Pair[Pattern, Rule]*) =
      ruls = rs.asInstanceOf[List[Pair[Pattern, Rule]]];

    def tryMatch =
      (ruls find { case Pair(p, _) => canMatch(p) }) match {
	case None => () => ();
	case Some(Pair(p, r)) => {
	  val args = values(p);
	  () => concurrent.ops.spawn(r(args))
	}
      }

  }

  /////////////////// SIGNALS /////////////////////////

  abstract class Signal (join: Join) {
    type C;
    val queue = new Queue[C];
    def tryReduction(x: C): unit = {
      val continuation = join synchronized {
	queue.enqueue(x);
	join.tryMatch
      };
      continuation()
    }
  }

  abstract class Asynchr(join: Join) extends Signal(join) {
    def send(x: C): unit = tryReduction(x);
  }

  abstract class Synchr[a](join: Join) extends Signal(join) {
    type C <: SyncVar[a];
    def send(x: C): a = {
      tryReduction(x);
      x.get
    }
  }

}

/* Example: References. (It does not work because of bugs in the compiler)

import concurrent.jolib._;
import concurrent.SyncVar;

class Ref(init: int) extends Join {

  object get extends Synchr[int](this) { case class C() extends SyncVar[int]; }
  object set extends Synchr[unit](this) { case class C(x: int) extends SyncVar[unit]; }
  object state extends Asynchr(this) { case class C(x: int); }

  rules (
    Pair(List(get, state), { case List(g @ get.C(), state.C(x) ) => { g.set(x); state.send(state.C(x)) } }),
    Pair(List(set, state), { case List(s @ set.C(x), state.C(y) ) => { s.set(()); state.send(state.C(x)) } })
  );

  state.send(state.C(init));

  def Get: int = get.send(get.C());
  def Set(x: int): unit = set.send(set.C(x));
}
*/

