package a.b
package c

import a.b.J

/** Variations of java-access-pos with us in a nested package.
 */

/** Declaring "override" all the time.
 */
class S1 extends J {
  override private[b] def packageAbstract() = ()      // fail
  override protected[b] def protectedAbstract() = ()
  override def publicAbstract() = ()
  
  override private[b] def packageConcrete() = ()      // fail
  override protected[b] def protectedConcrete() = ()
  override def publicConcrete() = ()
}

/** Implementing abstracts.
 */
class S2 extends J {
  private[b] def packageAbstract() = ()     // fail
  protected[b] def protectedAbstract() = ()
  def publicAbstract() = ()
}

/** Widening access.
 */
class S3 extends J {
  protected[b] def packageAbstract() = ()   // fail
  protected[b] def protectedAbstract() = ()
  def publicAbstract() = ()
  
  override protected[b] def packageConcrete() = ()    // fail
  override protected[b] def protectedConcrete() = ()
  override def publicConcrete() = ()
}
/** More widening.
 */
class S4 extends J {
  private[a] def packageAbstract() = ()         // fail
  protected[a] def protectedAbstract() = ()
  def publicAbstract() = ()
  
  override private[a] def packageConcrete() = ()        // fail
  override protected[a] def protectedConcrete() = ()
  override def publicConcrete() = ()
}
/** Yet more widening.
 */
class S5 extends J {
  def packageAbstract() = ()      // fail
  def protectedAbstract() = ()
  def publicAbstract() = ()
  
  override def packageConcrete() = ()   // fail
  override def protectedConcrete() = ()
  override def publicConcrete() = ()
}
