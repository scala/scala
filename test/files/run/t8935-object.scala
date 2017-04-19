import scala.tools.partest.SessionTest

import scala.tools.nsc.Settings

object Test extends SessionTest {
  /* future
  override def transformSettings(s: Settings): Settings = {
    //s.YreplWrap.value = "object"
    s
  }
  */
  def session =
"""
scala> 42
res0: Int = 42

scala> $intp.valueOfTerm($intp.mostRecentVar)
res1: Option[Any] = Some(42)

scala> val i = 17 ; 64
i: Int = 17
res2: Int = 64

scala> $intp.valueOfTerm($intp.mostRecentVar)
res3: Option[Any] = Some(64)

scala> $intp.valueOfTerm("i")
res4: Option[Any] = Some(17)

scala> :quit"""
}
