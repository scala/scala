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

object RefTest {

  def main(args: Array[String]) = {
    System.out.println("Started.");
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
  }

}
