object Test extends Application {
  val res0 = 1 #:: Stream.empty
  res0 match { case 1 #:: xs => xs }
}
