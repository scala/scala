object Test extends App {
  new Parent{}.foo(2)
  new Parent{}.bar(2)
  new Child{}.foo(2)
  new Child{}.bar(2)
}