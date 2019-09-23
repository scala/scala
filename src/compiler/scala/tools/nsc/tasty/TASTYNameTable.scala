package scala.tools.nsc.tasty

import TastyRefs.NameRef

trait TastyNameTable { self: TastyUniverse =>
  val nameAtRef: NameRef => self.symbolTable.TermName
  val signedNameAtRef: NameRef => Either[SignedName[self.symbolTable.TermName, self.symbolTable.TypeName], self.symbolTable.TermName]
}
