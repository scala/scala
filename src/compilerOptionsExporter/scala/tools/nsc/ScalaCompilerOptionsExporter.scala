/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import com.fasterxml.jackson.annotation._
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.{YAMLFactory, YAMLGenerator}
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.reflect.runtime.universe._

object ScalaCompilerOptionsExporter {

  case class Category(name: String, load: Int) extends Ordered[Category] {
    def compare(that: Category): Int = (this.load) compare (that.load)
  }
  val StandardSettings = Category("Standard Settings", 0)
  val JVMSettings = Category("JVM Settings", 1)
  val PluginSettings = Category("Plugin Settings", 2)
  val AdvancedSettings = Category("Advanced Settings", 3)
  val PrivateSettings = Category("Private Settings", 4)
  val WarningSettings = Category("Warning Settings", 5)
  val IDESpecificSettings = Category("IDE Specific Settings", 6)

  trait JacksonWorkaround {
     val category: String
  }
  @JsonIgnoreProperties(Array("_category"))
  @JsonPropertyOrder(Array("category", "description", "options"))
  case class Section(_category: Category, description: Option[String], options: List[ScalacOption]) extends JacksonWorkaround{
    val category: String = _category.name
  }
  case class ScalacOption(
    option: String,
    schema: Schema,
    description: String,
    abbreviations: Seq[String] = Seq.empty,
    deprecated: Option[String] = None,
    note: Option[String] = None
  )
  case class Schema(
    @JsonProperty("type") _type: String,
    arg: Option[String] = None,
    multiple: Option[Boolean] = None,
    default: Option[Any] = None,
    choices: Seq[Choice] = Seq.empty,
    min: Option[Any] = None,
    max: Option[Any] = None
  )
  case class Choice(choice: String, description: Option[String] = None, deprecated: Option[String] = None)

  private val quoted = """`([^`']+)'""".r

  def markdownifyBackquote(string: String) : String = {
    quoted.replaceAllIn(string, "`$1`")
  }

  private val htmlTag = """<([^>]+)>""".r
  def dehtmlfy(string: String) : String = {
    htmlTag.replaceAllIn(string, "$1")
  }

  def main(args: Array[String]): Unit = {
    val writer = new java.io.StringWriter(2000)

    val runtimeMirror = scala.reflect.runtime.currentMirror

    val settings = new scala.tools.nsc.Settings(s => ())
    val instanceMirror = runtimeMirror.reflect(settings)
    val sortedInOrderOfAppearance = runtimeMirror.classSymbol(settings.getClass).toType.members.sorted
    val accessors = sortedInOrderOfAppearance.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m
    }

    def mergeChoice(labels: Seq[String], descriptions: Seq[String]): Seq[Choice] = {
      for {
        (choice, d) <- (labels zipAll (descriptions, "", ""))
      } yield {
        Choice(
          choice,
          description = Option(d).map(markdownifyBackquote).map(dehtmlfy).filter(_.nonEmpty),
          deprecated = Some("EXPLAIN_ALTERNATIVE").filter(_ => d.toLowerCase.contains("deprecated"))
        )
      }
    }

    val extractedSettings : List[ScalacOption] = accessors.map(acc => instanceMirror.reflectMethod(acc).apply()).collect {
      case s: settings.Setting =>
        val schema = s match {
          case b: settings.BooleanSetting =>
            Schema(_type = "Boolean")
          case i: settings.IntSetting =>
            Schema(_type="Int", default = Some(i.default), min = i.range.map(_._1), max = i.range.map(_._2))
          case c: settings.ChoiceSetting =>
            val choices = mergeChoice(c.choices, c.choicesHelp)
            Schema(_type="Choice", arg = Some(c.helpArg).map(dehtmlfy), default = Option(c.default), choices = choices)
          case mc: settings.MultiChoiceSetting[_] =>
            val choices = mergeChoice(mc.choices, mc.descriptions)
            Schema(_type="Choice", multiple = Some(true), arg = Some(mc.helpArg).map(dehtmlfy), choices = choices)
          case ps: settings.PhasesSetting =>
            Schema(_type="Phases", default = Option(ps.default))
          case px: settings.PrefixSetting =>
            Schema(_type="Prefix")
          case sv: settings.ScalaVersionSetting =>
            Schema(_type="ScalaVersion", arg = Some(sv.arg).map(dehtmlfy), default = Some(sv.initial.unparse))
          case pathStr: settings.PathSetting =>
            Schema(_type="Path", arg = Some(pathStr.arg), default = Some(pathStr.default))
          case str: settings.StringSetting =>
            Schema(_type="String", arg = Some(str.arg).map(dehtmlfy), default = Some(str.default))
          case ms: settings.MultiStringSetting =>
            Schema(_type="String", multiple = Some(true), arg = Some(ms.arg).map(dehtmlfy))
        }

        ScalacOption(
          option = s.name,
          schema = schema,
          description = dehtmlfy(markdownifyBackquote(s.helpDescription)),
          abbreviations = s.abbreviations,
          deprecated = Some("EXPLAIN_ALTERNATIVE").filter(_ => s.helpDescription.toLowerCase.contains("deprecated"))
        )
    }


    val categoriezed = extractedSettings.groupBy { option =>
      val name = option.option
      if (name.startsWith("-Xfatal-warnings") || name == "-Xlint" || name.startsWith("-Ywarn")) {
        WarningSettings
      } else if (name.startsWith("-Ypresentation")) {
        IDESpecificSettings
      } else if (name.startsWith("-X")) {
        AdvancedSettings
      } else if (name.startsWith("-Y") || name.startsWith("-opt") && name != "-optimise") {
        PrivateSettings
      } else if (name.startsWith("-P")) {
        PluginSettings
      } else if (name.startsWith("-J") || name.startsWith("-D") || name.startsWith("-nobootcp")) {
        JVMSettings
      } else {
        StandardSettings
      }
    }

    val source = categoriezed.toSeq.sortBy(_._1).map { case (key, options) =>
      Section(key, Some("ADD_NICE_DESCRIPTION_HERE"),options = options)
    }

    val yamlFactory = new YAMLFactory()
      .disable(YAMLGenerator.Feature.SPLIT_LINES)
    val mapper = new ObjectMapper(yamlFactory)
      .registerModule(DefaultScalaModule)
      .setSerializationInclusion(JsonInclude.Include.NON_EMPTY)

    mapper
      .writer(new DefaultPrettyPrinter())
      .writeValue(writer, source)
    // TODO: println can be deleted if write can write to file
    println(writer.toString)
  }
}
