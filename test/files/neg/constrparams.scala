abstract class C(x: C) {
  type t;
  private val y: x.type = x;
  private val z: x.t = null; //error
}

