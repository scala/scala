/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package transform

class RuleTransformer(rules: RewriteRule*) extends BasicTransformer {
  override def transform(n: Node): Seq[Node] =
    rules.foldLeft(super.transform(n)) { (res, rule) => rule transform res }
}
