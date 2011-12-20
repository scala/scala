// This tests the implicit comment inheritance capabilities of scaladoc for usecases (no $super, no @inheritdoc)
/** Testing use case inheritance */
class UseCaseInheritance {
  /**
   * The base comment. And another sentence...
   * 
   * @param arg1 The T term comment
   * @param arg2 The string comment
   * @tparam T The type parameter
   * @return The return comment
   *
   * @usecase def missing_arg[T](arg1: T): Double
   *
   * @usecase def missing_targ(arg1: Int, arg2: String): Double
   *
   * @usecase def overridden_arg1[T](implicit arg1: T, arg2: String): Double
   * @param arg1 The overridden T term comment
   *
   * @usecase def overridden_targ[T](implicit arg1: T, arg2: String): Double
   * @tparam T The overridden type parameter comment
   *
   * @usecase def overridden_return[T](implicit arg1: T, arg2: String): Double
   * @return The overridden return comment
   *
   * @usecase def added_arg[T](implicit arg1: T, arg2: String, arg3: Float): Double
   * @param arg3 The added float comment
   *
   * @usecase def overridden_comment[T](implicit arg1: T, arg2: String): Double
   * The overridden comment.
   */ 
  def function[T](implicit arg1: T, arg2: String): Double = 0.0d
}

/** Testing the override-use case interaction */
class UseCaseOverrideInheritance extends UseCaseInheritance {
  /**
   * @usecase def missing_arg[T](arg1: T): Double
   *
   * @usecase def missing_targ(arg1: Int, arg2: String): Double
   *
   * @usecase def overridden_arg1[T](implicit arg1: T, arg2: String): Double
   * @param arg1 The overridden T term comment
   *
   * @usecase def overridden_targ[T](implicit arg1: T, arg2: String): Double
   * @tparam T The overridden type parameter comment
   *
   * @usecase def overridden_return[T](implicit arg1: T, arg2: String): Double
   * @return The overridden return comment
   *
   * @usecase def added_arg[T](implicit arg1: T, arg2: String, arg3: Float): Double
   * @param arg3 The added float comment
   *
   * @usecase def overridden_comment[T](implicit arg1: T, arg2: String): Double
   * The overridden comment.
   */ 
  override def function[T](implicit arg1: T, arg2: String): Double = 0.0d
} 
