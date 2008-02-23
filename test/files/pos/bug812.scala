package test;
import scala.{Application => Main};
class Test extends Main {
  import test.{Test => Hello}
  super[Application].main(Array("test"));
  private[Test] def xxx = 10;
}
