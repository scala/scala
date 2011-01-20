/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

class SeqCompletion[T](elems: Seq[T]) extends CompletionAware {
  lazy val completions = elems.indices.toList map ("(%d)" format _)
  def completions(verbosity: Int) = completions
  private def elemAt(name: String) =
    if (completions contains name) Some(elems(name drop 1 dropRight 1 toInt)) else None

  override def execute(name: String) = elemAt(name)
  override def follow(name: String) = elemAt(name) map (x => ProductCompletion(x))
}

/** TODO - deal with non-case products by giving them _1 _2 etc. */
class ProductCompletion(root: Product) extends CompletionAware {
  lazy val caseFields: List[Any] = root.productIterator.toList
  lazy val caseNames: List[String] = ByteCode caseParamNamesForPath root.getClass.getName getOrElse Nil
  private def isValid = caseFields.length == caseNames.length

  private def fieldForName(s: String) = (completions indexOf s) match {
    case idx if idx > -1 && isValid => Some(caseFields(idx))
    case _                          => None
  }

  lazy val completions = caseNames
  def completions(verbosity: Int) = completions
  override def execute(name: String) = fieldForName(name)
  override def follow(name: String) = fieldForName(name) map (x => ProductCompletion(x))
}

object ProductCompletion {
  /** TODO: other traversables. */
  def apply(elem: Any): CompletionAware = elem match {
    case x: Seq[_]    => new SeqCompletion[Any](x)
    case x: Product   => new ProductCompletion(x)
    // case x: Map[_, _] =>
    case _            => CompletionAware.Empty
  }
}
