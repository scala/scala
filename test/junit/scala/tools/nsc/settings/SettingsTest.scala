package scala.tools.nsc
package settings

import org.junit.Assert.{assertTrue => assert, _}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class SettingsTest {
  @Test def booleanSettingColon(): Unit = {
    def check(args: String*): MutableSettings#BooleanSetting = {
      val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
      val b1 = new s.BooleanSetting("-Ytest-setting", "")
      s.allSettings(b1.name) = b1
      val (ok, residual) = s.processArguments(args.toList, processAll = true)
      assert(residual.isEmpty)
      b1
    }
    assert(check("-Ytest-setting").value)
    assert(check("-Ytest-setting:true").value)
    assert(check("-Ytest-setting:TRUE").value)
    assertFalse(check("-Ytest-setting:false").value)
    assertFalse(check("-Ytest-setting:FALSE").value)
    assertThrows[IllegalArgumentException](check("-Ytest-setting:rubbish"))
  }

  // for the given args, select the desired setting
  private def check(args: String*)(b: MutableSettings => Boolean): Boolean = {
    val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
    val (ok, residual) = s.processArguments(args.toList, processAll = true)
    assert(residual.isEmpty)
    b(s)
  }

  @Test def `deprecation and xlint deprecation play well together`(): Unit = {
    assert(check("-deprecation")(_.deprecation))
    assert(check("--deprecation")(_.deprecation))
    assert(check("-deprecation")(!_.lintDeprecation))
    assert(check("-Xlint:deprecation")(_.deprecation))
    assert(check("-Xlint:deprecation")(_.lintDeprecation))
    assert(check("-deprecation", "-Xlint:deprecation")(ss => ss.deprecation && ss.lintDeprecation))
    assert(check("-deprecation", "-Xlint:-deprecation")(ss => ss.deprecation && !ss.lintDeprecation))
    // normally, explicit yes overrides explicit no, but here we treat them as different settings.
    // lint will enable deprecation if not explicitly disabled, otherwise just lints the usage.
    assert(check("-deprecation:false", "-Xlint:deprecation")(ss => !ss.deprecation && ss.lintDeprecation))
    assert(check("-deprecation:false", "-Xlint:-deprecation")(ss => !ss.deprecation && !ss.lintDeprecation))
    // same behavior with -Xlint:_
    assert(check("-deprecation", "-Xlint")(ss => ss.deprecation && ss.lintDeprecation))
    assert(check("-deprecation", "-Xlint:-deprecation,_")(ss => ss.deprecation && !ss.lintDeprecation))
    assert(check("-deprecation:false", "-Xlint")(ss => !ss.deprecation && ss.lintDeprecation))
  }

  @Test def userSettingsHavePrecedenceOverLint(): Unit = {
    assert(check("-Xlint")(_.warnUnusedImport))
    assertFalse(check("-Xlint", "-Ywarn-unused:-imports")(_.warnUnusedImport))
    assertFalse(check("-Ywarn-unused:-imports", "-Xlint")(_.warnUnusedImport))
  }

  @Test def anonymousLintersCanBeNamed(): Unit = {
    assert(check("-Xlint")(_.warnMissingInterpolator)) // among Xlint
    assertFalse(check("-Xlint:-missing-interpolator")(_.warnMissingInterpolator))

    // positive overrides negative, but not the other way around
    assert(check("-Xlint:-missing-interpolator,missing-interpolator")(_.warnMissingInterpolator))
    assert(check("-Xlint:-missing-interpolator", "-Xlint:missing-interpolator")(_.warnMissingInterpolator))

    assert(check("-Xlint:missing-interpolator,-missing-interpolator")(_.warnMissingInterpolator))
    assert(check("-Xlint:missing-interpolator", "-Xlint:-missing-interpolator")(_.warnMissingInterpolator))

    // -Xlint:_ adds all possible choices, but explicit negative settings will override
    assertFalse(check("-Xlint:-missing-interpolator,_")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint:-missing-interpolator", "-Xlint:_")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint:_", "-Xlint:-missing-interpolator")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint:_,-missing-interpolator")(_.warnMissingInterpolator))

    // -Xlint is the same as -Xlint:_
    assertFalse(check("-Xlint:-missing-interpolator", "-Xlint")(_.warnMissingInterpolator))
    assertFalse(check("-Xlint", "-Xlint:-missing-interpolator")(_.warnMissingInterpolator))

    // combination of positive, negative and _
    assert(check("-Xlint:_,-missing-interpolator,missing-interpolator")(_.warnMissingInterpolator))
    assert(check("-Xlint:-missing-interpolator,_,missing-interpolator")(_.warnMissingInterpolator))
    assert(check("-Xlint:-missing-interpolator,missing-interpolator,_")(_.warnMissingInterpolator))
    assert(check("-Xlint:missing-interpolator,-missing-interpolator,_")(_.warnMissingInterpolator))
    assert(check("-Xlint:missing-interpolator,_,-missing-interpolator")(_.warnMissingInterpolator))
  }

  @Test def xLintInvalidChoices(): Unit = {
    assertThrows[IllegalArgumentException](check("-Xlint:-_")(_.warnAdaptedArgs))
    assertThrows[IllegalArgumentException](check("-Xlint:-warn-adapted-args")(_.warnAdaptedArgs)) // "warn-" should not be there
  }

  @Test def xLintNonColonated(): Unit = {
    assert(check("-Xlint", "adapted-args", "-deprecation")(_.warnAdaptedArgs))
    assertFalse(check("-Xlint", "adapted-args", "-deprecation")(_.warnMissingInterpolator))
    assert(check("-Xlint", "adapted-args", "missing-interpolator", "-deprecation")(s => s.warnMissingInterpolator && s.warnAdaptedArgs))
    assertThrows[IllegalArgumentException](check("-Xlint", "adapted-args", "-missing-interpolator")(_.warnAdaptedArgs)) // non-colonated: cannot provide negative args
  }

  @Test def xLintContainsValues(): Unit = {
    // make sure that lint.contains and lint.value.contains are consistent
    def t(s: MutableSettings, v: String) = {
      val r = s.lint.contains(v)
      assertSame(r, s.lint.value.contains((s.LintWarnings withName v).asInstanceOf[s.lint.domain.Value]))
      r
    }

    assert(check("-Xlint")(t(_, "adapted-args")))
    assert(check("-Xlint:_")(t(_, "adapted-args")))
    assertFalse(check("-Xlint:_,-adapted-args")(t(_, "adapted-args")))
    assertFalse(check("-Xlint:-adapted-args,_")(t(_, "adapted-args")))
    assert(check("-Xlint:-adapted-args,_,adapted-args")(t(_, "adapted-args")))
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

    assert(check("-m")(_.value == Set(a,c)))
    assert(check("-m:a,-b,c")(_.value == Set(a,c)))

    // expanding options don't end up in the value set, only the terminal ones
    assert(check("-m:ab,ac")(_.value == Set(a,b,c)))
    assert(check("-m:_")(_.value == Set(a,b,c,d)))
    assert(check("-m:uber,ac")(_.value == Set(a,b,c,d))) // recursive expansion of uber

    // explicit nays
    assert(check("-m:_,-b")(_.value == Set(a,c,d)))
    assert(check("-m:b,_,-b")(_.value == Set(a,b,c,d)))
    assert(check("-m:ac,-c")(_.value == Set(a)))
    assert(check("-m:ac,-a,-c")(_.value == Set()))
    assert(check("-m:-d,ac")(_.value == Set(a,c)))
    assert(check("-m:-b,ac,uber")(_.value == Set(a,c,d)))

    assertFalse(check("-m:uber")(_.contains("i-m-not-an-option")))

    assertThrows[IllegalArgumentException](check("-m:-_")(_ => true), _ contains "'-_' is not a valid choice")
    assertThrows[IllegalArgumentException](check("-m:a,b,-ab")(_ => true), _ contains "'ab' cannot be negated")
    assertThrows[IllegalArgumentException](check("-m:a,ac,-uber,uber")(_ => true), _ contains "'uber' cannot be negated")
  }

  @Test def xSourceTest(): Unit = {
    def check(expected: String, args: String*): Unit = {
      val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
      val (_, residual) = s.processArguments(args.toList, processAll = true)
      assert(s"remaining input [$residual]", residual.isEmpty)
      assert(s"(${s.source.value} == ${ScalaVersion(expected)})", s.source.value == ScalaVersion(expected))
    }
    check(expected = "2.13.0") // default
    check(expected = "9.11.0", "-Xsource:9.11")
    check(expected = "9.10",   "-Xsource:9.10.0")
    check(expected = "9.12",   "-Xsource:9.12")
    check(expected = "9.13",   "-Xsource:9.13")
    assertThrows[IllegalArgumentException](check(expected = "2.12", "-Xsource:2.12"), _ == "-Xsource must be at least the current major version (2.13.0)")
    assertThrows[IllegalArgumentException](check(expected = "2.11", "-Xsource"), _ == "-Xsource requires an argument, the syntax is -Xsource:<version>")
    assertThrows[IllegalArgumentException](check(expected = "2.11", "-Xsource", "2.11"), _ == "-Xsource requires an argument, the syntax is -Xsource:<version>")
    assertThrows[IllegalArgumentException](check(expected = "2.11", "-Xsource:2.invalid"), _ contains "Bad version (2.invalid)")
  }

  // equal with stripped margins and normalized line endings
  private def marginallyEquals(s1: String, s2: String): Boolean = {
    def normally(s: String): String = s.stripMargin.linesIterator.mkString("\n")
    normally(s1) == normally(s2)
  }

  @Test def helpHasDefault(): Unit = {
    val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
    object mChoices extends s.MultiChoiceEnumeration {
      val a = Choice("a", "help a")
      val b = Choice("b", "help b")
      val c = Choice("c", "help c")
    }
    val m = s.MultiChoiceSetting("-m", "args", "magic sauce", mChoices, Some(List("b")))

    def check(args: String*)(t: s.MultiChoiceSetting[mChoices.type] => Boolean): Boolean = {
      m.clear()
      val (ok, rest) = s.processArguments(args.toList, processAll = true)
      assert(rest.isEmpty)
      t(m)
    }

    import mChoices._

    assert(check("-m")(_.value == Set(b)))
    assert(check("-m") { _ =>
      val expected =
        """|magic sauce
           |  a  help a
           |  b  help b
           |  c  help c
           |Default: b
           |"""
      marginallyEquals(expected, m.help)
    })
  }
  @Test def helpHasDefaultAll(): Unit = {
    val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
    object mChoices extends s.MultiChoiceEnumeration {
      val a = Choice("a", "help a")
      val b = Choice("b", "help b")
      val c = Choice("c", "help c")
    }
    val m = s.MultiChoiceSetting("-m", "args", "magic sauce", mChoices, Some(List("_")))

    def check(args: String*)(t: s.MultiChoiceSetting[mChoices.type] => Boolean): Boolean = {
      m.clear()
      val (ok, rest) = s.processArguments(args.toList, processAll = true)
      assert(rest.isEmpty)
      t(m)
    }

    import mChoices._

    assert(check("-m")(_.value == Set(a, b, c)))
    assert(check("-m") { _ =>
      val expected =
        """|magic sauce
           |  a  help a
           |  b  help b
           |  c  help c
           |Default: All choices are enabled by default.
           |"""
      marginallyEquals(expected, m.help)
    })
  }
  @Test def `wildcard doesn't disable everything`(): Unit = {
    val settings = new Settings()
    settings.processArguments("-opt:_" :: Nil, true)
    assert("has the choice", settings.opt.contains(settings.optChoices.inline))
    assert("is enabled", settings.optInlinerEnabled)
  }
  @Test def `kill switch can be enabled explicitly`(): Unit = {
    val settings = new Settings()
    settings.processArguments("-opt:inline,l:none" :: Nil, true)
    assert("has the choice", settings.opt.contains(settings.optChoices.inline))
    assertFalse("is not enabled", settings.optInlinerEnabled)
  }
}
