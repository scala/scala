import scala.xml._

import scala.xml.transform._

object Test {
  val sampleXml = <p><lost/><t><s><r></r></s></t></p>

  def main(args: scala.Array[String]) {

    println(new RuleTransformer(new RewriteRule {

        override def transform(n: Node): NodeSeq = { 
          val result = n match {
          case <t>{_*}</t> => <q/>

          case n => n

          }
//          println ("Rewriting '" +n+ "' to: '" + result+ "'")

          result
        }
      }).transform(sampleXml))
  }
}
