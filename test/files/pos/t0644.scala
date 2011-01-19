class A {
  def apply(): Int = 0
  def update(n: Int) {}
}

class B extends A {
  this()
  this()=1
  // 644 is wontfix so this is what should work.
  super.apply()
  super.update(1)
}
