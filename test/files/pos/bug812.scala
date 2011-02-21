package test;
import scala.{App => Main};
class Test extends Main {
  import test.{Test => Hello}
  super[App].main(Array("test"));
  private[Test] def xxx = 10;
}
