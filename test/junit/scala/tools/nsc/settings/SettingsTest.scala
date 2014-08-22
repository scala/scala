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
  private def check(args: String*)(b: MutableSettings => Boolean): Boolean = {
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

    // positive overrides negative, but not the other way around
    assertTrue(check("-Xlint:-missing-interpolator,missing-interpolator")(_.warnMissingInterpolator))
    assertTrue(check("-Xlint:-missing-interpolator", "-Xlint:missing-interpolator")(_.warnMissingInterpolator))

    assertTrue(check("-Xlint:missing-interpolator,-missing-interpolator")(_.warnMissingInterpolator))
    assertTrue(check("-Xlint:missing-interpolator", "-Xlint:-missing-interpolator")(_.warnMissingInterpolator))

    // -Xlint:_ adds all possible choices, but explicit negative settings will override
    assertFalse(check("-Xlint:-missing-interpolator,_")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint:-missing-interpolator", "-Xlint:_")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint:_", "-Xlint:-missing-interpolator")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint:_,-missing-interpolator")(_.warnMissingInterpolator))

    // -Xlint is the same as -Xlint:_
    assertFalse(check("-Xlint:-missing-interpolator", "-Xlint")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint", "-Xlint:-missing-interpolator")(_.warnMissingInterpolator))

    // combination of positive, negative and _
    assertTrue(check("-Xlint:_,-missing-interpolator,missing-interpolator")(_.warnMissingInterpolator))
    assertTrue(check("-Xlint:-missing-interpolator,_,missing-interpolator")(_.warnMissingInterpolator))
    assertTrue(check("-Xlint:-missing-interpolator,missing-interpolator,_")(_.warnMissingInterpolator))
    assertTrue(check("-Xlint:missing-interpolator,-missing-interpolator,_")(_.warnMissingInterpolator))
    assertTrue(check("-Xlint:missing-interpolator,_,-missing-interpolator")(_.warnMissingInterpolator))
  }

  @Test def xLintInvalidChoices(): Unit = {
    assertThrows[IllegalArgumentException](check("-Xlint:-_")(_.warnAdaptedArgs))
    assertThrows[IllegalArgumentException](check("-Xlint:-warn-adapted-args")(_.warnAdaptedArgs)) // "warn-" should not be there
  }

  @Test def xLintNonColonated(): Unit = {
    assertTrue(check("-Xlint", "adapted-args", "-deprecation")(_.warnAdaptedArgs))
    assertFalse(check("-Xlint", "adapted-args", "-deprecation")(_.warnMissingInterpolator))
    assertTrue(check("-Xlint", "adapted-args", "missing-interpolator", "-deprecation")(s => s.warnMissingInterpolator && s.warnAdaptedArgs))
    assertThrows[IllegalArgumentException](check("-Xlint", "adapted-args", "-missing-interpolator")(_.warnAdaptedArgs)) // non-colonated: cannot provide negative args
  }

  @Test def xLintContainsValues(): Unit = {
    // make sure that lint.contains and lint.value.contains are consistent
    def t(s: MutableSettings, v: String) = {
      val r = s.lint.contains(v)
      assertSame(r, s.lint.value.contains(v))
      r
    }

    assertTrue(check("-Xlint")(t(_, "adapted-args")))
    assertTrue(check("-Xlint:_")(t(_, "adapted-args")))
    assertFalse(check("-Xlint:_,-adapted-args")(t(_, "adapted-args")))
    assertFalse(check("-Xlint:-adapted-args,_")(t(_, "adapted-args")))
    assertTrue(check("-Xlint:-adapted-args,_,adapted-args")(t(_, "adapted-args")))
  }

  @Test def xLintDeprecatedAlias(): Unit = {
    assertTrue(check("-Ywarn-adapted-args")(_.warnAdaptedArgs))
    assertTrue(check("-Xlint:_,-adapted-args", "-Ywarn-adapted-args")(_.warnAdaptedArgs))
    assertTrue(check("-Xlint:-adapted-args", "-Ywarn-adapted-args")(_.warnAdaptedArgs))
    assertTrue(check("-Ywarn-adapted-args", "-Xlint:-adapted-args,_")(_.warnAdaptedArgs))

    assertFalse(check("-Ywarn-adapted-args:false")(_.warnAdaptedArgs))
    assertFalse(check("-Ywarn-adapted-args:false", "-Xlint:_")(_.warnAdaptedArgs))
    assertFalse(check("-Ywarn-adapted-args:false", "-Xlint:_,-adapted-args")(_.warnAdaptedArgs))
    assertTrue(check("-Ywarn-adapted-args:false", "-Xlint:_,adapted-args")(_.warnAdaptedArgs))
  }
}
