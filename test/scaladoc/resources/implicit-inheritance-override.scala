// This tests the implicit comment inheritance capabilities of scaladoc for class inheritance (no $super, no @inheritdoc)
class Base {
  /**
   * The base comment. And another sentence...
   *
   * @param  arg1 The T term comment
   * @param  arg2 The string comment
   * @tparam T the type of the first argument
   * @return The return comment
   */
  def function[T](arg1: T, arg2: String): Double = 0.0d
}

class DerivedA extends Base {
  /**
   * Overriding the comment, the params and returns comments should stay the same.
   */
  override def function[T](arg1: T, arg2: String): Double = 1.0d
}

class DerivedB extends Base {
  /**
   * @param arg1 The overridden T term comment
   * @param arg2 The overridden string comment
   */
  override def function[T](arg1: T, arg2: String): Double = 2.0d
}

class DerivedC extends Base {
  /**
   * @return The overridden return comment
   */
  override def function[T](arg1: T, arg2: String): Double = 3.0d
}

class DerivedD extends Base {
  /**
   * @tparam T The overridden type parameter comment
   */
  override def function[T](arg1: T, arg2: String): Double = 3.0d
}