package tastytest

import givens._

object TestTCGivens {
  def exported = TCModule.TC.mkTCFromInt[1]
  def original: TCInstances.TC.mkTCFromInt[1] = TCInstances.TC.mkTCFromInt[1]
}
