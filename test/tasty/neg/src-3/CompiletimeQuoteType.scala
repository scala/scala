package tastytest

import scala.quoted._

object CompiletimeQuoteType {
  def f[T: Type](using QuoteContext) = {
    implicitly[Type[List[T]]]
  }
}
