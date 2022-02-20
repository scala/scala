package scala.tools.nsc
package settings

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class SettingsTest {
  private def settings = new MutableSettings(msg => throw new IllegalArgumentException(msg))

  @Test def booleanSettingColon(): Unit = {
    def check(args: String*): MutableSettings#BooleanSetting = {
      val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
      val b1 = new s.BooleanSetting("-Ytest-setting", descr="", default=false)
      s.allSettings(b1.name) = b1
      val (ok, residual) = s.processArguments(args.toList, processAll = true)
      assertTrue(residual.isEmpty)
      b1
    }
    assertTrue(check("-Ytest-setting").value)
    assertTrue(check("-Ytest-setting:true").value)
    assertTrue(check("-Ytest-setting:TRUE").value)
    assertFalse(check("-Ytest-setting:false").value)
    assertFalse(check("-Ytest-setting:FALSE").value)
    assertThrows[IllegalArgumentException](check("-Ytest-setting:rubbish"))
  }

  // for the given args, select the desired setting
  private def check(args: String*)(b: MutableSettings => Boolean): Boolean = {
    val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
    val (ok, residual) = s.processArguments(args.toList, processAll = true)
    assertTrue(residual.isEmpty)
    b(s)
  }

  @Test def `deprecation and xlint deprecation play well together`(): Unit = {
    assertTrue(check("-deprecation")(_.deprecation))
    assertTrue(check("--deprecation")(_.deprecation))
    assertTrue(check("-deprecation")(!_.lintDeprecation))
    assertTrue(check("-Xlint:deprecation")(_.deprecation))
    assertTrue(check("-Xlint:deprecation")(_.lintDeprecation))
    assertTrue(check("-deprecation", "-Xlint:deprecation")(ss => ss.deprecation && ss.lintDeprecation))
    assertTrue(check("-deprecation", "-Xlint:-deprecation")(ss => ss.deprecation && !ss.lintDeprecation))
    // normally, explicit yes overrides explicit no, but here we treat them as different settings.
    // lint will enable deprecation if not explicitly disabled, otherwise just lints the usage.
    assertTrue(check("-deprecation:false", "-Xlint:deprecation")(ss => !ss.deprecation && ss.lintDeprecation))
    assertTrue(check("-deprecation:false", "-Xlint:-deprecation")(ss => !ss.deprecation && !ss.lintDeprecation))
    // same behavior with -Xlint:_
    assertTrue(check("-deprecation", "-Xlint")(ss => ss.deprecation && ss.lintDeprecation))
    assertTrue(check("-deprecation", "-Xlint:-deprecation,_")(ss => ss.deprecation && !ss.lintDeprecation))
    assertTrue(check("-deprecation:false", "-Xlint")(ss => !ss.deprecation && ss.lintDeprecation))
  }

  @Test def userSettingsHavePrecedenceOverLint(): Unit = {
    assertTrue(check("-Xlint")(_.warnUnusedImport))
    assertFalse(check("-Xlint", "-Ywarn-unused:-imports")(_.warnUnusedImport))
    assertFalse(check("-Ywarn-unused:-imports", "-Xlint")(_.warnUnusedImport))
  }

  @Test def anonymousLintersCanBeNamed(): Unit = {
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
      assertTrue(rest.isEmpty)
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
      assertTrue(s"remaining input [$residual]", residual.isEmpty)
      assertTrue(s"(${s.source.value} == ${ScalaVersion(expected)})", s.source.value == ScalaVersion(expected))
    }
    check(expected = "2.13.0") // default
    check(expected = "3",      "-Xsource:2.14")
    check(expected = "3",      "-Xsource:2.15")
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
      assertTrue(rest.isEmpty)
      t(m)
    }

    import mChoices._

    assertTrue(check("-m")(_.value == Set(b)))
    assertTrue(check("-m") { _ =>
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
      assertTrue(ok)
      assertTrue(rest.isEmpty)
      t(m)
    }

    import mChoices._

    assertTrue(check("-m")(_.value == Set(a, b, c)))
    assertTrue(check("-m") { _ =>
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
    assertTrue("has the choice", settings.opt.contains(settings.optChoices.unreachableCode))
    assertTrue("is enabled", settings.optUnreachableCode)
    assertFalse("inliner is not enabled", settings.optInlinerEnabled)
  }
  @Test def `kill switch can be enabled explicitly`(): Unit = {
    val settings = new Settings()
    settings.processArguments("-opt:unreachable-code,none" :: Nil, true)
    assertTrue("has the choice", settings.opt.contains(settings.optChoices.unreachableCode))
    assertFalse("is not enabled", settings.optUnreachableCode)
  }
  @Test def `kill switch disables inline`(): Unit = {
    val settings = new Settings()
    settings.processArguments("-opt:inline:**" :: "-opt:none" :: Nil, true)
    assertTrue("has the choice", settings.optInlineFrom.nonEmpty)
    assertFalse("is not enabled", settings.optInlinerEnabled)
  }
  @Test def `t12036 don't consume dash option as arg`(): Unit = {
    import scala.collection.mutable.ListBuffer
    val errors   = ListBuffer.empty[String]
    val settings = new Settings(errors.addOne)
    val (ok, rest) = settings.processArguments("-Vinline" :: "-Xlint" :: Nil, true)
    assertFalse("processing should fail", ok)
    assertEquals("processing stops at bad option", 2, rest.length)
    assertEquals(2, errors.size)  // missing arg and bad option
  }
  @Test def `t12098 MultiStringSetting with prepend handles non-colon args`(): Unit = {
    import scala.collection.mutable.ListBuffer
    val errors   = ListBuffer.empty[String]
    val settings = new Settings(errors.addOne)
    val (ok, rest) = settings.processArguments("-Wconf" :: "help" :: "-Vdebug" :: "x.scala" :: Nil, true)
    assertTrue("processing should succeed", ok)
    assertEquals("processing stops at argument", 1, rest.length)
    assertEquals("processing stops at the correct argument", "x.scala", rest.head)
    assertEquals(0, errors.size)
    assertTrue(settings.debug)
    assertTrue(settings.Wconf.isHelping)
  }
  @Test def `t12098 MultiStringSetting prepends`(): Unit = {
    val settings = new Settings(msg => fail(s"Unexpected error: $msg"))
    val (ok, rest) = settings.processArguments("-Wconf:cat=lint-missing-interpolator:ws" :: "-Xlint" :: "x.scala" :: Nil, true)
    assertTrue("processing should succeed", ok)
    assertTrue(settings.warnMissingInterpolator)
    assertTrue(settings.lintDeprecation)
    // test/files/neg/t12098.scala shows that cat=deprecation:w due to xlint supersedes default cat=deprecation:ws
  }
  @Test def `choices can be multichoices`(): Unit = {
    val s = new MutableSettings(msg => throw new IllegalArgumentException(msg))
    object mChoices extends s.MultiChoiceEnumeration {
      val a = Choice("a")
      val b = Choice("b")
      val c = Choice("c")
      val d = Choice("d")
    }
    val m = s.MultiChoiceSetting("-m", "args", "magic sauce", mChoices, Some(List("a")))
    println(s"m $m has value ${m.value}")

    def check(args: String*)(t: s.MultiChoiceSetting[mChoices.type] => Boolean): Boolean = {
      m.clear()
      val (ok, rest) = s.processArguments(args.toList, processAll = true)
      assertTrue(ok)
      assertTrue(rest.isEmpty)
      t(m)
    }

    import mChoices._

    assertTrue(check("-m:a:aval")(choices => choices.value == Set(a) && choices.value.toList.head.asInstanceOf[mChoices.Choice].selections == List("aval")))
    assertTrue(check("-m:a:aval1,aval2")(choices => choices.value == Set(a) && choices.value.toList.head.asInstanceOf[mChoices.Choice].selections == List("aval1", "aval2")))
  }
  @Test def `optimizer flags are sane`: Unit = {
    val s = settings
    val args = "-opt:inline:p.*" :: Nil
    val (ok, rest) = s.processArguments(args.toList, processAll = true)
    assertTrue(ok)
    assertTrue(rest.isEmpty)
    assertTrue(s.optInlinerEnabled)
    assertEquals("p.*" :: Nil, s.optInlineFrom)
  }
  @Test def `optimizer inline patterns are additive`: Unit = {
    val s = settings
    val args = "-opt:inline:p.*" :: "-opt:inline:q.C.m" :: Nil
    val (ok, rest) = s.processArguments(args.toList, processAll = true)
    assertTrue(ok)
    assertTrue(rest.isEmpty)
    assertTrue(s.optInlinerEnabled)
    assertEquals("p.*" :: "q.C.m" :: Nil, s.optInlineFrom)
  }
  @Test def `optimizer old flags are supported`: Unit = {
    val s = settings
    val args = "-opt:l:method" :: Nil
    val (ok, rest) = s.processArguments(args.toList, processAll = true)
    assertTrue(ok)
    assertTrue(rest.isEmpty)
    assertFalse(s.optInlinerEnabled)
    assertTrue(s.optBoxUnbox)
  }
}
