package a.b

/** Declaring "override" all the time.
 */
class S1 extends J {
  override private[b] def packageAbstract() = ()
  override protected[b] def protectedAbstract() = ()
  override def publicAbstract() = ()

  override private[b] def packageConcrete() = ()
  override protected[b] def protectedConcrete() = ()
  override def publicConcrete() = ()
}

/** Implementing abstracts.
 */
class S2 extends J {
  private[b] def packageAbstract() = ()
  protected[b] def protectedAbstract() = ()
  def publicAbstract() = ()
}

/** Widening access.
 */
class S3 extends J {
  protected[b] def packageAbstract() = ()
  protected[b] def protectedAbstract() = ()
  def publicAbstract() = ()

  override protected[b] def packageConcrete() = ()
  override protected[b] def protectedConcrete() = ()
  override def publicConcrete() = ()
}
/** More widening.
 */
class S4 extends J {
  private[a] def packageAbstract() = ()
  protected[a] def protectedAbstract() = ()
  def publicAbstract() = ()

  override private[a] def packageConcrete() = ()
  override protected[a] def protectedConcrete() = ()
  override def publicConcrete() = ()
}
/** Yet more widening.
 */
class S5 extends J {
  def packageAbstract() = ()
  def protectedAbstract() = ()
  def publicAbstract() = ()

  override def packageConcrete() = ()
  override def protectedConcrete() = ()
  override def publicConcrete() = ()
}
/** Constructors.
 */
class S6 extends J(1) {
  def packageAbstract() = ()
  def protectedAbstract() = ()
  def publicAbstract() = ()
}
class S7 extends J(1, 2) {
  def packageAbstract() = ()
  def protectedAbstract() = ()
  def publicAbstract() = ()
}