// $Id$

abstract class Bug360A: Bug360C {
  def f: String = "hello";
}
abstract class Bug360B: Bug360C {
  object d {
    System.out.println(f);
  }
}
abstract class Bug360C extends Bug360A with Bug360B;
