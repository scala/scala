object Test extends App {

  import scala.util.Properties._

  setProp("java.version", "1.7.0_17")

  //assert( isJavaAtLeast(""))
  //assert( isJavaAtLeast("1"))
  //assert( isJavaAtLeast("1.4.2"))
  assert( isJavaAtLeast("1.5"))
  //assert( isJavaAtLeast("1.5.0_16"))
  //assert( isJavaAtLeast("1.5.0_23"))
  assert( isJavaAtLeast("1.6"))
  //assert( isJavaAtLeast("1.6.0_1"))
  assert( isJavaAtLeast("1.7"))
  //assert(isJavaAtLeast("1.7.0_16"))
  //assert(isJavaAtLeast("1.7.0_17"))
  //assert(!isJavaAtLeast("1.7.0_18"))
  //assert(isJavaAtLeast("1.7.0-internal"))
  assert(!isJavaAtLeast("1.8"))
  //assert(!isJavaAtLeast("1.8.0_1"))
  //assert(!isJavaAtLeast("1.8.0-internal"))
  //assert(!isJavaAtLeast("12.53.12_24"))
}
