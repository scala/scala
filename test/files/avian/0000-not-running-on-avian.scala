object Test extends App {
  val vmName = scala.util.Properties.javaVmName
  if (vmName contains "Avian")
    println("SUCCESS: Test is running on Avian.")
  else
    println(explanation)

  def explanation =
s"""TEST FAILED.

This test (and very likely every other test as well) is not
running on Avian, it is running on: $vmName.
Tests in this directory are intended to be run on Avian only.

If the Ant build script claims it is running on Avian,
but this test fails, there is a high chance that you are
running Ant on Avian, but the tests themselves get executed
on something different.

On systems with multiple JREs/JDKs installed, these mistakes
are common:

- Not setting up PATH properly
- Not setting up JAVA_HOME properly
- Not setting up LD_PRELOAD properly (if necessary)

This error message is provided in the hope that the next
person won't waste days of his/her life on this."""
}
