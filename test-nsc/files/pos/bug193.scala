// $Id$

trait Test {

  def fun_00(x: Int): Unit = {
    (0: Any) == 0;
    (0     ) == 0;
    (0: Any) != 0;
    (0     ) != 0;
    ()
  }

  def fun_i0(x: Int): Unit = {
    (x: Any) == 0;
    (x     ) == 0;
    (x: Any) != 0;
    (x     ) != 0;
    ()
  }

  def fun_o0(x: Object): Unit = {
    (x: Any) == 0;
    (x     ) == 0;
    (x: Any) != 0;
    (x     ) != 0;
    ()
  }

  def fun_0i(y: Int): Unit = {
    (0: Any) == y;
    (0     ) == y;
    (0: Any) != y;
    (0     ) != y;
    ()
  }

  def fun_0o(y: Object): Unit = {
     (0: Any) == y;
     (0     ) == y;
     (0: Any) != y;
     (0     ) != y;
    ()
  }

  def fun_ii(x: Int, y: Int): Unit = {
    (x: Any) == y;
    (x     ) == y;
    (x: Any) != y;
    (x     ) != y;
    ()
  }
  def fun_io(x: Int, y: Object): Unit = {
    (x: Any) == y;
    (x     ) == y;
    (x: Any) != y;
    (x     ) != y;
    ()
  }
  def fun_oi(x: Object, y: Int): Unit = {
    (x: Any) == y;
    (x     ) == y;
    (x: Any) != y;
    (x     ) != y;
    ()
  }
  def fun_oo(x: Object, y: Object): Unit = {
    (x: Any) == y;
    (x     ) == y;
    (x: Any) != y;
    (x     ) != y;
    ()
  }

}
