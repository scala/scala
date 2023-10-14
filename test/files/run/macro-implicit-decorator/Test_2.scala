// https://github.com/scala/bug/issues/10398

class CustomClass

trait MyTC[A]

object MyTC {
  implicit val forInt: MyTC[Int] = new MyTC[Int] {}
  implicit def forList[A](implicit a: Derivation[MyTC[A]]): MyTC[List[A]] = new MyTC[List[A]] {}
  implicit def forCustomClass(implicit a: Derivation[MyTC[List[Boolean]]]): MyTC[CustomClass] = new MyTC[CustomClass] {}
}

object Test extends App {
  println(implicitly[Derivation[MyTC[List[Int]]]])
  println(implicitly[Derivation[MyTC[List[Boolean]]]])
  println(implicitly[Derivation[MyTC[CustomClass]]])
}
