object Test extends App {
  Macros.bar(2)
  Macros.bar("2")
  new Macros.bar(2)
  new Macros.bar("2")
}