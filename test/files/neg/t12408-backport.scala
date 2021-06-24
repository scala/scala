// scalac: -Xsource:2.13 -Werror
class A[X] { def f[Y](x: Option[Y]) = x match { case s: Some[X] => 0; case _ => 1 } }
