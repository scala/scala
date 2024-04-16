//> using options -deprecation -Wunused:nowarn -Yrangepos:false -Werror
import scala.annotation._

class ann(a: Any) extends Annotation

class C {
  @deprecated("message", "1.2.3") def dep = 0

  @nowarn def t0 = { 0: @nowarn; 1 }     // outer @nowarn unused
  @nowarn def t1 = { 0: Int @nowarn; 1 } // inner @nowarn unused, it covers the type, not the expression
  @nowarn @ann(dep) def t2 = 0          // deprecation warning, @nowarn unused
  @ann(dep: @nowarn) def t3 = 0         // silent

  @nowarn("cat=deprecation") def t4 = dep

  def t5 = (new I1a).m

  // completion forced by method above
  @nowarn class I1a { // unused @nowarn
    @nowarn def m = { 1; 2 }
  }

  // completion during type checking
  @nowarn class I1b { // unused @nowarn
    @nowarn def m = { 1; 2 }
  }

  def t6 = (new I1b).m

  @nowarn val t7a = { 0; 1 }
  val t7b = { 0; 1 }

  @nowarn class I2a {
    def f: Unit = 1
  }
  class I2b {
    def f: Unit = 1
  }

  trait I3a
  @nowarn object I3a {
    def main(args: Array[String]) = ()
  }
  trait I3b
  object I3b {
    def main(args: Array[String]) = () // main method in companion of trait triggers a warning in the backend
  }

  def t8(): Unit = {
    @nowarn
    val a = {
      123
      ()
    }
    val b = {
      123
      ()
    }
  }

  @nowarn("msg=pure expression")
  def t9a(): Unit = {
    123
  }
  @nowarn("msg=something else")
  def t9b(): Unit = {
    123
  }

  @nowarn
  def t10a(): Unit = {
    123
  }
  def t10b(): Unit = {
    123
  }
  
  def t11(): Unit = {
    val a = dep: @nowarn
    a + dep
  }
}

trait T {
  @nowarn val t1 = { 0; 1 }
}

class K extends T

@nowarn("site=Uh.f.g")
class Uh {
  def f = {
    def g(c: C) = c.dep
  }
}
