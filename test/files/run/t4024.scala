object Test extends Application {

  val x = "abc"

  val m = x.getClass.getMethod("toString")

  assert(m.invoke(x, (Nil: List[AnyRef]): _*) == "abc")
}

