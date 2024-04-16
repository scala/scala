//> using javaOpt -Dneeds.forked.jvm
// bug #1062
object Test extends App {
  println((new MethVsField_1).three)
}
