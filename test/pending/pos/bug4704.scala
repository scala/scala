trait Bar {
  def f1 = super.hashCode
  def f2 = super[Object].hashCode
  def f3 = super[ScalaObject].hashCode
  
  override def hashCode = 1
}
trait Barzoo {
  def g1 = super.hashCode
  def g2 = super[Object].hashCode
  def g3 = super[ScalaObject].hashCode
  
  override def hashCode = 2
}

trait Foo extends Bar with Barzoo {
  def f4 = super.hashCode
  def f5 = super[Object].hashCode
  def f6 = super[ScalaObject].hashCode
  def f6b = super[Bar].hashCode
  def g4 = super[Barzoo].hashCode
  
  override def hashCode = super[Bar].hashCode + super[Barzoo].hashCode
}

class Quux extends Foo {
  override def hashCode = super.hashCode + super[Object].hashCode + super[ScalaObject].hashCode + super[Foo].hashCode
}

trait Borp extends Quux {
  def f12 = super.hashCode
  def f14 = super[ScalaObject].hashCode
  def f15 = super[Quux].hashCode
  override def hashCode = super[Quux].hashCode
}

