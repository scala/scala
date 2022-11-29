// scalac: -Werror -Xsource:3

import collection.AbstractIterable

abstract class LIterable[+A] extends AbstractIterable[A] {
  def iterator(): Iterator[A]
}

object Test extends App {
  val chars: LIterable[Char] = () => "hello, world".iterator
}
