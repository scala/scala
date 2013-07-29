object Test extends App {
  println(Module.value)
  Module.value = "world"
  println(Module.value)
}