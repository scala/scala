package foo

trait Base {
  private def somePublicMethod(): Int = 123
}

final class BaseClass extends Base
