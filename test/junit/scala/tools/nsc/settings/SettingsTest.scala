package scala.tools.nsc
package settings

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.testing.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class SettingsTest {
  @Test def booleanSettingColon() {
    def check(args: String*): MutableSettings#BooleanSetting = {
      val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
      val b1 = new s.BooleanSetting("-Ytest-setting", "")
      s.allSettings += b1
      val (ok, residual) = s.processArguments(args.toList, processAll = true)
      assert(residual.isEmpty)
      b1
    }
    assertTrue(check("-Ytest-setting").value)
    assertTrue(check("-Ytest-setting:true").value)
    assertTrue(check("-Ytest-setting:TRUE").value)
    assertFalse(check("-Ytest-setting:false").value)
    assertFalse(check("-Ytest-setting:FALSE").value)
    assertThrows[IllegalArgumentException](check("-Ytest-setting:rubbish"))
  }

  @Test def userSettingsHavePrecedenceOverOptimize() {
    def check(args: String*): MutableSettings#BooleanSetting = {
      val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
      val (ok, residual) = s.processArguments(args.toList, processAll = true)
      assert(residual.isEmpty)
      s.inline // among -optimize
    }
    assertTrue(check("-optimise").value)
    assertFalse(check("-optimise", "-Yinline:false").value)
    assertFalse(check("-Yinline:false", "-optimise").value)
  }

  // for the given args, select the desired setting
  private def check(args: String*)(b: MutableSettings => MutableSettings#BooleanSetting): MutableSettings#BooleanSetting = {
    val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
    val (ok, residual) = s.processArguments(args.toList, processAll = true)
    assert(residual.isEmpty)
    b(s)
  }
  @Test def userSettingsHavePrecedenceOverLint() {
    assertTrue(check("-Xlint")(_.warnAdaptedArgs))
    assertFalse(check("-Xlint", "-Ywarn-adapted-args:false")(_.warnAdaptedArgs))
    assertFalse(check("-Ywarn-adapted-args:false", "-Xlint")(_.warnAdaptedArgs))
  }

  @Test def anonymousLintersCanBeNamed() {
    assertTrue(check("-Xlint")(_.warnMissingInterpolator)) // among Xlint
    assertFalse(check("-Xlint:-missing-interpolator")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint:-missing-interpolator", "-Xlint")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint", "-Xlint:-missing-interpolator")(_.warnMissingInterpolator))
  }
}
