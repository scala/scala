package foo

trait Base {
  def somePublicMethod(): Int = 123
}

final class BaseClass extends Base
