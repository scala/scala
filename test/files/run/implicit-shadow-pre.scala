class C {
  implicit def foo: String = this.toString;
}

case object Test extends C {
   case object Inner extends C {
      println(foo)
      println(implicitly[String])
   }

   def main(args: Array[String]) {
      Inner
   }
}
