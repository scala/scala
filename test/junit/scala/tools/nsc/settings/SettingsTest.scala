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

  @Test def userSettingsHavePrecedenceOverExperimental() {
    def check(args: String*): MutableSettings#BooleanSetting = {
      val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
      val (ok, residual) = s.processArguments(args.toList, processAll = true)
      assert(residual.isEmpty)
      s.YpartialUnification // among -Xexperimental
    }
    assertTrue(check("-Xexperimental").value)
    assertFalse(check("-Xexperimental", "-Ypartial-unification:false").value)
    assertFalse(check("-Ypartial-unification:false", "-Xexperimental").value)
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
      assertSame(r, s.lint.value.contains((s.LintWarnings withName v).asInstanceOf[s.lint.domain.Value]))
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

  @Test def expandingMultichoice(): Unit = {
    val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
    object mChoices extends s.MultiChoiceEnumeration {
      val a = Choice("a")
      val b = Choice("b")
      val c = Choice("c")
      val d = Choice("d")

      val ab = Choice("ab", expandsTo = List(a, b))
      val ac = Choice("ac", expandsTo = List(a, c))
      val uber = Choice("uber", expandsTo = List(ab, d))
    }
    val m = s.MultiChoiceSetting("-m", "args", "magic sauce", mChoices, Some(List("ac")))

    def check(args: String*)(t: s.MultiChoiceSetting[mChoices.type] => Boolean): Boolean = {
      m.clear()
      val (ok, rest) = s.processArguments(args.toList, processAll = true)
      assert(rest.isEmpty)
      t(m)
    }

    import mChoices._

    assertTrue(check("-m")(_.value == Set(a,c)))
    assertTrue(check("-m:a,-b,c")(_.value == Set(a,c)))

    // expanding options don't end up in the value set, only the terminal ones
    assertTrue(check("-m:ab,ac")(_.value == Set(a,b,c)))
    assertTrue(check("-m:_")(_.value == Set(a,b,c,d)))
    assertTrue(check("-m:uber,ac")(_.value == Set(a,b,c,d))) // recursive expansion of uber

    // explicit nays
    assertTrue(check("-m:_,-b")(_.value == Set(a,c,d)))
    assertTrue(check("-m:b,_,-b")(_.value == Set(a,b,c,d)))
    assertTrue(check("-m:ac,-c")(_.value == Set(a)))
    assertTrue(check("-m:ac,-a,-c")(_.value == Set()))
    assertTrue(check("-m:-d,ac")(_.value == Set(a,c)))
    assertTrue(check("-m:-b,ac,uber")(_.value == Set(a,c,d)))

    assertFalse(check("-m:uber")(_.contains("i-m-not-an-option")))

    assertThrows[IllegalArgumentException](check("-m:-_")(_ => true), _ contains "'-_' is not a valid choice")
    assertThrows[IllegalArgumentException](check("-m:a,b,-ab")(_ => true), _ contains "'ab' cannot be negated")
    assertThrows[IllegalArgumentException](check("-m:a,ac,-uber,uber")(_ => true), _ contains "'uber' cannot be negated")
  }

  @Test def xSourceTest(): Unit = {
    def check(expected: String, args: String*): Unit = {
      val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
      val (_, residual) = s.processArguments(args.toList, processAll = true)
      assert(residual.isEmpty)
      assertTrue(s.source.value == ScalaVersion(expected))
    }
    check(expected = "2.12.0") // default
    check(expected = "2.11.0", "-Xsource:2.11")
    check(expected = "2.10",   "-Xsource:2.10.0")
    check(expected = "2.12",   "-Xsource:2.12")
    assertThrows[IllegalArgumentException](check(expected = "2.11", "-Xsource"), _ == "-Xsource requires an argument, the syntax is -Xsource:<version>")
    assertThrows[IllegalArgumentException](check(expected = "2.11", "-Xsource", "2.11"), _ == "-Xsource requires an argument, the syntax is -Xsource:<version>")
    assertThrows[IllegalArgumentException](check(expected = "2.11", "-Xsource:2.invalid"), _ contains "Bad version (2.invalid)")
  }
}
