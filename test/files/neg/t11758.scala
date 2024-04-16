//> using options -Xlint:deprecation -Wunused:imports -Werror

import language.higherKinds

@deprecated("no outer", "1.0")
object outer {
  @deprecated("no inner", "2.0")
  object inner
  object other
}
object nest {
  @deprecated("no see", "3.0")
  class C

  val status = true
}

trait T {
  import outer.other
  import outer.inner
  import outer.{inner => odder}

  def f = inner

  import nest.C
  def g = ()
}
trait U {
  import nest.C
  def g = new C
}
trait OK {
  import nest.{C => _, _}
  def ok = status
}
