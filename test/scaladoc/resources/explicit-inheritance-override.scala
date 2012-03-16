// This tests the implicit comment inheritance capabilities of scaladoc for class inheritance (no $super, no @inheritdoc)
class InheritDocBase {
  /**
   * The base comment. And another sentence...
   *
   * @param  arg1 The T term comment
   * @param  arg2 The string comment
   * @tparam T the type of the first argument
   * @throws SomeException if the function is not called with correct parameters
   * @return The return comment
   * @see The Manual
   * @note Be careful!
   * @example function[Int](3, "something")
   * @author a Scala developer
   * @version 0.0.2
   * @since 0.0.1
   * @todo Call mom.
   */
  def function[T](arg1: T, arg2: String): Double = 0.0d
}

class InheritDocDerived extends InheritDocBase {
  /**
   * Starting line
   *
   * @inheritdoc
   * @inheritdoc
   *
   * Ending line
   *
   * @param arg1 Start1 @inheritdoc End1
   * @param arg2 Start2 @inheritdoc End2
   * @param arg3 Start3 ShouldWarn @inheritdoc End3
   * @tparam T StartT @inheritdoc EndT
   * @tparam ShouldWarn StartSW @inheritdoc EndSW
   * @throws SomeException StartEx @inheritdoc EndEx
   * @throws SomeOtherException StartSOE Should Warn @inheritdoc EndSOE
   * @return StartRet @inheritdoc EndRet
   * @see StartSee @inheritdoc EndSee
   * @note StartNote @inheritdoc EndNote
   * @example StartExample @inheritdoc EndExample
   * @author StartAuthor @inheritdoc EndAuthor
   * @version StartVer @inheritdoc EndVer
   * @since StartSince @inheritdoc EndSince
   * @todo StartTodo @inheritdoc And dad! EndTodo
   */
  override def function[T](arg1: T, arg2: String): Double = 1.0d
}