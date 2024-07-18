//> using jvm 11+
//
// skalac: --release:8
// trivial manual test for partest --realeasy, which sets --release:8.
// under --realeasy, skip this test because of the javaVersion, irrespective of JDK in use.
// otherwise, this test passes trivially on JDK11+ and is skipped on lesser JDKs.
// note that explicit --release:8 asks to compile against JDK8 but only run on the requested version.

object Test extends App {
  assert("".isBlank)  // String#isBlank was added in JDK11
}
