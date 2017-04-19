/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.reflect.internal

import org.junit.{ Ignore, Test }
import scala.tools.reflect._
import scala.reflect.runtime.{ currentMirror => cm }
import org.junit.Assert._

// For easier test result validation
trait Stringable {
  override def toString = getClass.getSimpleName
}

class TraitResult extends Stringable
class ClassResult extends Stringable

trait TraitDep extends Stringable {
  private val privateTraitField = "privateTraitField"
  protected val protectedTraitField = "protectedTraitField"
  val publicTraitField = "publicTraitField"

  private def privateTraitMethod = "traitMethod"

  protected def protectedTraitMethod = "traitMethod"

  def publicTraitMethod = "publicTraitMethod"

  private val privateTraitProtectedClass = new TraitResult
  protected val protectedTraitPrivateClass = new TraitResult
  private val privateTraitPrivateClass = new TraitResult
  val publicTraitPrivateClass = new TraitResult
  private val privateTraitPublicClass = new TraitResult
}

class ClassDep extends Stringable {
  private val privateClassField = "privateClassField"
  protected val protectedClassField = "protectedClassField"
  val publicClassField = "publicClassField"

  private def privateClassMethod = "classMethod"

  protected def protectedClassMethod = "classMethod"

  def publicClassMethod = "publicClassMethod"

  protected val privateTraitProtectedClass =new  ClassResult
  private val protectedTraitPrivateClass = new ClassResult
  private val privateTraitPrivateClass = new ClassResult
  private val publicTraitPrivateClass = new ClassResult
  val privateTraitPublicClass = new ClassResult
}

object Libs extends ClassDep with TraitDep {
  private val privateObjectField = "privateObjectField"
  protected val protectedObjectField = "protectedObjectField"
  val publicObjectField = "publicObjectField"

  private def privateObjectMethod = "objectMethod"

  protected def protectedObjectMethod = "objectMethod"

  def publicObjectMethod = "publicObjectMethod"
}

class PrivateAccess {

  lazy val toolbox = cm.mkToolBox(options = "-Yignore-access-mods")

  private def addImports(code: String) = s"import scala.reflect.internal._ \n $code"

  def shouldOnlyTypcheck(code: String, expectedType: Option[String] = None) = {
    val fullCode = addImports(code)
    val parsed = toolbox.parse(fullCode)
    val tree = toolbox.typecheck(parsed)

    expectedType.foreach(expected => assertEquals(expected, tree.tpe.toString))

    try {
      val parsed2 = toolbox.parse(fullCode)
      toolbox.compile(parsed2)
      fail("Expression should not compile. Flag should works only in typecheck")
    } catch {
      case te: ToolBoxError =>
    }
  }

  def shouldCompile(code: String, expectedType: Option[String] = None): Unit = {
    val parsed = toolbox.parse(addImports(code))
    val tree = toolbox.typecheck(parsed)

    expectedType.foreach(expected => assertEquals(expected, tree.tpe.toString))

    val parsed2 = toolbox.parse(addImports(code))
    toolbox.compile(parsed2)
  }

  def checkResult(code: String, result: String): Unit = {
    val parsed = toolbox.parse(addImports(code))
    val compiled = toolbox.compile(parsed)

    assertEquals("Result differs", compiled().toString, result)
  }

  @Ignore("Add support for private member from parent classes/traits")
  @Test
  def privateTraitField(): Unit = shouldOnlyTypcheck("Libs.privateTraitField")

  @Ignore("Add support for private member from parent classes/traits")
  @Test
  def privateClassField(): Unit = shouldOnlyTypcheck("Libs.privateClassField")

  @Test
  def privateObjectField(): Unit = shouldOnlyTypcheck("Libs.privateObjectField")

  @Ignore("Add support for private member from parent classes/traits")
  @Test
  def privateTraitMethod(): Unit = shouldOnlyTypcheck("Libs.privateTraitMethod")

  @Ignore("Add support for private member from parent classes/traits")
  @Test
  def privateClassMethod(): Unit = shouldOnlyTypcheck("Libs.privateClassMethod")

  @Test
  def privateObjectMethod(): Unit = shouldOnlyTypcheck("Libs.privateObjectMethod")

  @Test
  def testProtectedMember(): Unit = shouldOnlyTypcheck("Libs.protectedTraitField")

  @Test
  def protectedClassField(): Unit = shouldOnlyTypcheck("Libs.protectedClassField")

  @Test
  def protectedObjectField(): Unit = shouldOnlyTypcheck("Libs.protectedObjectField")


  @Test
  def protectedTraitMethod(): Unit = shouldOnlyTypcheck("Libs.protectedTraitMethod")

  @Test
  def protectedClassMethod(): Unit = shouldOnlyTypcheck("Libs.protectedClassMethod")

  @Test
  def protectedObjectMethod(): Unit = shouldOnlyTypcheck("Libs.protectedObjectMethod")

  @Test
  def importedProtectedObjectMethod(): Unit = shouldOnlyTypcheck("import Libs._; protectedObjectMethod")

  @Test
  def publicTraitField(): Unit = checkResult("Libs.publicTraitField", "publicTraitField")

  @Test
  def publicClassField(): Unit = checkResult("Libs.publicClassField", "publicClassField")

  @Test
  def publicObjectField(): Unit = checkResult("Libs.publicObjectField", "publicObjectField")

  @Test
  def publicTraitMethod(): Unit = checkResult("Libs.publicTraitMethod", "publicTraitMethod")

  @Test
  def publicClassMethod(): Unit = checkResult("Libs.publicClassMethod", "publicClassMethod")

  @Test
  def publicObjectMethod(): Unit = checkResult("Libs.publicObjectMethod", "publicObjectMethod")

  @Test
  def privateTraitProtectedClass(): Unit =
    shouldOnlyTypcheck("Libs.privateTraitProtectedClass", Some("scala.reflect.internal.ClassResult"))

  @Test
  def protectedTraitPrivateClass(): Unit =
    shouldOnlyTypcheck("Libs.protectedTraitPrivateClass", Some("scala.reflect.internal.TraitResult"))

  @Ignore("Add support for private member from parent classes/traits")
  @Test
  def privateTraitPrivateClass(): Unit =
    shouldOnlyTypcheck("Libs.privateTraitPrivateClass", Some("scala.reflect.internal.ClassResult"))

  @Test
  def publicTraitPrivateClass(): Unit =
    shouldCompile("Libs.publicTraitPrivateClass", Some("scala.reflect.internal.TraitResult"))

  @Test
  def privateTraitPublicClass(): Unit =
    shouldCompile("Libs.privateTraitPublicClass", Some("scala.reflect.internal.ClassResult"))
}
