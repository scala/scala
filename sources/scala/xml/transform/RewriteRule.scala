package scala.xml.transform ;

/** a RewriteRule, when applied to a term, yields either
 *  the resulting of rewriting or the term itself it the rule
 *  is not applied
 */
abstract class RewriteRule extends BasicTransformer {
  /** a name for this rewrite rule */
  val name = this.toString();
  override def transform(ns:Seq[Node]): Seq[Node] = super.transform(ns);
  override def transform(n:Node): Seq[Node] = n;
}

