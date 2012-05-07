package test

object Compat {
  final val CONST_STRING = "constant"
  lazy val foo = 4
}

class Foo {
  Compat./*!*/CONST_STRING // its 'accessible' flag is false
  Compat./*!*/foo // its 'accessible' flag is false
}
