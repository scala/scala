object Test extends Application {
  val p = new Proxy {
    def self = 2 
  }
  println(p equals 1)
  println(p equals 2)
  println(p equals 3)
  println(p equals null)
}
