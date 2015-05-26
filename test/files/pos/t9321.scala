object p {
  trait A {
    private[p] val qualifiedPrivateMember = 1
  }
 
  def useQualifiedPrivate(b: Mix) =
    b.qualifiedPrivateMember // allowed
}
 
trait Mix extends p.A
