

import scala.tools.partest.SessionTest

// Taking unary ids as plain
object Test extends SessionTest {
  def session =
"""Type in expressions to have them evaluated.
Type :help for more information.

scala> val - = 42
-: Int = 42

scala> val i = -
i: Int = 42

scala> - { 42 }
res0: Int = -42

scala> - if (true) 1 else 2
<console>:1: error: illegal start of simple expression
- if (true) 1 else 2
  ^

scala> - - 1
<console>:1: error: ';' expected but integer literal found.
- - 1
    ^

scala> -.-(1)
res1: Int = 41

scala> -
res2: Int = 42

scala> - -
res3: Int = -42

scala> + -
res4: Int = 42

scala> object X { def -(i: Int) = 42 - i ; def f(g: Int => Int) = g(7) ; def j = f(-) }
defined object X

scala> X.j
res5: Int = 35

scala> :quit"""
}
