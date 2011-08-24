trait C {
  def fun1(def x: Int): Int = x; // the "def x" is illegal
  def fun2(val x: Int): Int = x; // the "val x" is illegal
}

class E(def x: Int); // the "def x" is illegal
