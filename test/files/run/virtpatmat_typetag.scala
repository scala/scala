import reflect.{ClassTag, classTag}

trait Extractors {
  type T
  implicit val tTag: ClassTag[T]
  object ExtractT {
    def unapply(x: T) = Some(x)
  }
  def apply(a: Any) = a match {
    case ExtractT(x)  => println(x +" is a "+ implicitly[ClassTag[T]])
    case _ => println(a+ " is not a "+ implicitly[ClassTag[T]] +"; it's a "+ a.getClass)
  }
}

object Test extends App {
  def typeMatch[T: ClassTag](a: Any) = a match {
    case x : T => println(x +" is a "+ implicitly[ClassTag[T]])
    case _ => println(a+ " is not a "+ implicitly[ClassTag[T]] +"; it's a "+ a.getClass)
  }

  // the same match as typeMatch, but using an extractor
  def extractorMatch[S: ClassTag](a: Any) =
    (new Extractors { type T = S; val tTag = classTag[T] })(a)

  typeMatch[Int](1)
  typeMatch[Integer](1)
  typeMatch[String](1)
  typeMatch[Any](true)
  typeMatch[String]("woele")

  extractorMatch[Int](1)
  extractorMatch[Integer](1)
  extractorMatch[String](1)
  extractorMatch[Any](true)
  extractorMatch[String]("woele")
}