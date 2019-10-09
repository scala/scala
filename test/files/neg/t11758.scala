
// scalac: -Xlint:deprecation -Wunused:imports -Werror

import language.higherKinds

@deprecated("no outer", "")
object outer {
  @deprecated("no inner", "")
  object inner
  object other
}
object nest {
  @deprecated("no see", "")
  class C

  val status = true
}

trait T {
  import outer.other
  import outer.inner
  import outer.{inner => odder}

  def f = inner

  import nest.C
  def g = new C
}
trait OK {
  import nest.{C => _, _}
  def ok = status
}
