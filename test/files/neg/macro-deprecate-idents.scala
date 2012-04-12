object Test1 {
  val macro = ???
}

object Test2 {
  var macro = ???
}

object Test3 {
  type macro = Int
}

package test4 {
  class macro
}

object Test5 {
  class macro
}

package test6 {
  object macro
}

object Test7 {
  object macro
}

package test8 {
  trait macro
}

object Test9 {
  trait macro
}

package macro {
  package macro.bar {
  }
}

package foo {
  package macro.foo {
  }
}

object Test12 {
  val Some(macro) = Some(42)
  macro match {
    case macro => println(macro)
  }
}

object Test13 {
  def macro = 2
}