//> using options -Wvalue-discard -Werror

import annotation._

//the following option works and warns for f but not g
//-Wconf:cat=w-flag-value-discard&site=T.g:s

@nowarn("cat=w-flag-value-discard&site=T.g")
trait T {
  def v(): Int
  def f(): Unit = v()
  def g(): Unit = v()
}
// was: no warning
