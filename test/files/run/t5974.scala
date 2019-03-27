object Test extends App {
  import scala.jdk.CollectionConverters.Ops._

  def ser(a: AnyRef) =
    (new java.io.ObjectOutputStream(new java.io.ByteArrayOutputStream())).writeObject(a)

  val l = java.util.Arrays.asList("pigdog").asScala
  ser(l)
  println("ok")
}
