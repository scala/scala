package scala.reflect
package std

import internal._

abstract class Mirror extends SymbolTable
                         with JavaMappings
                         with SymbolCompleters
                         with Positions { self =>

  val unpickler = new internal.pickling.UnPickler { val global: self.type = self }
  override val settings = new ReflectSettings

}
