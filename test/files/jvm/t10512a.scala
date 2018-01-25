trait JsonValue
class JsonObject extends JsonValue
class JsonString extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

object JsonEncoderInstances {

  val seWorks: JsonEncoder[String] =
    new JsonEncoder[String] {
      def encode(value: String) = new JsonString
    }

  implicit val stringEncoder: JsonEncoder[String] =
    s => new JsonString
    //new JsonEncoder[String] {
    //  def encode(value: String) = new JsonString
    //}

  def leWorks[A](implicit encoder: JsonEncoder[A]): JsonObjectEncoder[List[A]] =
    new JsonObjectEncoder[List[A]] {
      def encode(value: List[A]) = new JsonObject
    }

  implicit def listEncoder[A](implicit encoder: JsonEncoder[A]): JsonObjectEncoder[List[A]] =
    l => new JsonObject
//    new JsonObjectEncoder[List[A]] {
//      def encode(value: List[A]) = new JsonObject
//    }

}

object Test extends App {
  import JsonEncoderInstances._

  implicitly[JsonEncoder[List[String]]].encode("" :: Nil)
}