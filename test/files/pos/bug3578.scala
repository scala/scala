object Test {
  sealed abstract class JValue {
    def ++(other: JValue) = {
      def append(value1: JValue, value2: JValue): JValue = (value1, value2) match {
	case (JNothing, x) => x
	case (x, JNothing) => x
	case (JObject(xs), x: JField) => JObject(xs ::: List(x))
	case (x: JField, JObject(xs)) => JObject(x :: xs)
	case (JArray(xs), JArray(ys)) => JArray(xs ::: ys)
	case (JArray(xs), v: JValue) => JArray(xs ::: List(v))
	case (v: JValue, JArray(xs)) => JArray(v :: xs)
	case (f1: JField, f2: JField) => JObject(f1 :: f2 :: Nil)
	case (JField(n, v1), v2: JValue) => JField(n, append(v1, v2))
	case (x, y) => JArray(x :: y :: Nil)
      }
      append(this, other)
    }
  }

  case object JNothing extends JValue
  case object JNull extends JValue
  case class JString(s: String) extends JValue
  case class JDouble(num: Double) extends JValue
  case class JInt(num: BigInt) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JField(name: String, value: JValue) extends JValue
  case class JObject(obj: List[JField]) extends JValue 
  case class JArray(arr: List[JValue]) extends JValue
}

