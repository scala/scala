// See Test.scala for comments

trait T { def f = 1 }

class A {
  // member class
  class B
  // member trait
  trait C
  // member object
  object D {
    class B
    trait C
    object D
    new T { }
    (() => "-1")
    def f = { class KB }
  }

  // anonymous class, not a member
  new T { }

  // anonymous function, not a member
  (() => "1")

  def f = {
    class E
    trait F
    object G
    new T { }
    (() => "2")

    if (new Object().hashCode == 1) {
      class H
      trait I
      object J
      new T { }
      (() => "3")
    } else {
      ()
    }
  }

  {
    class K
    trait L
    object M
    new T { }
    (() => "4")
  }

  val x = {
    class N
    trait O
    object P
    new T { }
    (() => "5")
  }

  def this(x: Int) {
    this()
    class Q
    trait R
    object S
    new T { }
    (() => () => "5")
  }
}

object AO {
  class B
  trait C
  object D
  new T { }
  (() => "1")
}

trait AT {
  class B
  trait C
  object D
  new T { }
  (() => "1")
}
