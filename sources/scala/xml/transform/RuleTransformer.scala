package scala.xml.transform;

class RuleTransformer(rules:RewriteRule*) extends BasicTransformer {
  override def transform(n:Node): Seq[Node] = {
    var m: Seq[Node] = super.transform(n);
    val it = rules.elements; while(it.hasNext) {
      val rule = it.next;
      val m2 = rule.transform(m);
      //if(!m2.eq(m)) Console.println("applied rule \""+rule.name+"\"");
      m = m2;
    }
    m
  }
}
