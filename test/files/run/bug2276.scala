import scala.xml._
import scala.xml.transform._

object Test extends App {
  val inputXml : Node = 
    <root>
      <subnode>
        <version>1</version>
      </subnode>
      <contents>
        <version>1</version>
      </contents>
    </root>

  object t1 extends RewriteRule {
    override def transform(n: Node): Seq[Node] = n match {
      case <version>{x}</version> if x.toString.toInt < 4 => <version>{x.toString.toInt+1}</version>
      case other => other
    }
  }

  val ruleTransformer = new RuleTransformer(t1)
  println(ruleTransformer(inputXml))
}
