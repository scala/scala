import scala.runtime.NativeMonitor;

case class Foo[a](x: a);

object bug {
  Foo("");
  NativeMonitor.synchronised(null, "");
}
