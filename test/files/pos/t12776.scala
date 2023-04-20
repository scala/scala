
import java.util.concurrent.CompletableFuture

class C {
  def f = new CompletableFuture[String].handle((s, t) => ())
}
