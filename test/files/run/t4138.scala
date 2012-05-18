object Test extends App {
  object p extends scala.util.parsing.combinator.JavaTokenParsers

  println(p.parse(p.stringLiteral, """"lir 'de\' ' \\ \n / upa \"new\" \t parsing""""))
  println(p.parse(p.stringLiteral, """"s " lkjse""""))
}
