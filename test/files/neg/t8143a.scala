class Foo { def f = 5 }
class Bar extends Foo { private def f = 10 }


class Foo1 { private def f = 5 }
class Bar1 extends Foo1 { def f = 10 } // okay

class Foo2 { private def f = 5 }
class Bar2 extends Foo2 { private def f = 10 } // okay

class Foo3 { private[this] def f = 5 }
class Bar3 extends Foo3 { private def f = 10 } // okay

class Foo4 { private def f = 5 }
class Bar4 extends Foo4 { private[this] def f = 10 } // okay