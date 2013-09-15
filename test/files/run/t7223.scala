class D(val a: () => Int => () => Any) {
  a()(0)()
}

object Crash extends D(() => {
  (x: Int) => {() => { new { println(x.toString) } }}
})

object Test extends App {
  Crash
}
