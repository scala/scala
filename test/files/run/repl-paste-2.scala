import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
scala> 123
res0: Int = 123

scala> 567
res1: Int = 567

scala> res0 + res1
res2: Int = 690

scala> val x = dingus
<console>:7: error: not found: value dingus
       val x = dingus
               ^

scala> val x = "dingus"
x: java.lang.String = dingus

scala> x.length
res3: Int = 6

scala> x.length + res3
res4: Int = 12
  """
}