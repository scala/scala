
//> using options -deprecation -Wconf:cat=deprecation&origin=C:s -Werror -Xlint

@deprecated("Just say no.", since="1.0")
class C

@deprecated("Will be phased out eventually someday.", since="2.0")
class D

trait Test {
  def c = new C
  def d = new D
}
