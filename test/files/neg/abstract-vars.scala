abstract class A {
  var x: Int
}

class Fail1 extends A {
  var x: Int
}

class Fail2 extends A { }

class Fail3 extends A {
  val x: Int = 5
}
class Fail4 extends A {
  def x: Int = 5
}

class Fail5 extends A {
  def x_=(y: Int) = ()
}

class Success1 extends A {
  val x: Int = 5
  def x_=(y: Int) = ()
}

class Success2 extends A {
  var x: Int = 5
}
