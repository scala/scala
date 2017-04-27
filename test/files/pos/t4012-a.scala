trait C1[+A] {
  def head: A = sys.error("")
}
trait C2[@specialized +A] extends C1[A] {
  override def head: A = super.head
}
class C3 extends C2[Char] 
