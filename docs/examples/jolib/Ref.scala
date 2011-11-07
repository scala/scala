/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package examples.jolib;
/*
import concurrent.SyncVar;
import concurrent.jolib._;

class Ref[a](init: a) extends Join {
  
  object get extends Synchr[a](this) { case class C() extends SyncVar[a]; }
  object set extends Synchr[unit](this) { case class C(x: a) extends SyncVar[unit]; }
  object state extends Asynchr(this) { case class C(x: a); }

  rules (
    Pair(List(get, state), { case List(g @ get.C(), state.C(x) ) =>
      { g.set(x); state(state.C(x)) } }),
    Pair(List(set, state), { case List(s @ set.C(x), state.C(y) ) =>
      { s.set(()); state(state.C(x)) } })
  );

  state(state.C(init));
  
  def Get: a = get(get.C());
  def Set(x: a): unit = set(set.C(x));
}
*/
object RefTest {

  def main(args: Array[String]) = {
    System.out.println("Started.");
/*
    concurrent.ops.spawn({
      val r1 = new Ref(0);
      System.out.println("Reference r1 created.");
      System.out.println("Value r1 (first time) = " + r1.Get);
      r1.Set(42);
      System.out.println("Value r1 (second time) = " + r1.Get);
    });
    concurrent.ops.spawn({
      val r2 = new Ref(100);
      System.out.println("Reference r2 created.");
      System.out.println("Value r2 (first time) = " + r2.Get);
      r2.Set(89);
      System.out.println("Value r2 (second time) = " + r2.Get);
    });
*/
  }

}
