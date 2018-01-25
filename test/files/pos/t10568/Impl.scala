package y

import x._

class Impl extends Converter.FactoryFactory {
  import Converter.FactoryFactory._
  def method: String =
    getString + Converter.STRING
}