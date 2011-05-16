package scala.reflect
package std

import common._

abstract class Mirror extends SymbolTable
                         with JavaMappings
                         with SymbolCompleters
                         with Positions { self =>

  val unpickler = new common.pickling.UnPickler { val global: self.type = self }
  override val settings = new ReflectSettings

}
