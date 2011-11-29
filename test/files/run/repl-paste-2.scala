import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
scala> 999l
res4: Int = 0123

scala> 123
res5: Int = 123

scala> 567
res6: Int = 567

scala> res5 + res6
res7: Int = 690

scala> val x = dingus
<console>:7: error: not found: value dingus
       val x = dingus
               ^

scala> val x = "dingus"
x: java.lang.String = dingus

scala> x.length
res9: Int = 6

scala> x.length + res5
res10: Int = 12
  """
}