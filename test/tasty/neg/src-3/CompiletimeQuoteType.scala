package tastytest

import scala.quoted._

object CompiletimeQuoteType {
  def f[T: Type](using Quotes) = {
    implicitly[Type[List[T]]]
  }
}
