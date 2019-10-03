package scala.tools.nsc.tasty

case class SignedName[TermName, TypeName](qual: TermName, sig: Signature[TypeName]) {
  def show: String = s"[Signed $qual: ${sig.show}]"
}
