package p {
  object Crash {
    def e(s: (String @java.lang.Deprecated)): Unit = ()
    def f(s: (String @nonStatic)): Unit = ()
  }
  object Ok {
    def g(s: (String @nonStatic @static)): Unit = ()
    def h(s: (String @static)): Unit = ()
  }
}

class nonStatic extends scala.annotation.Annotation
class static extends scala.annotation.StaticAnnotation
