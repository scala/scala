/* NSC -- new Scala compiler
 * Copyright 2007-2010 LAMP/EPFL
 * @author  Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package model

import scala.collection._
import comment._

/** Some entity of the Scaladoc model. */
trait Entity {
  def name : String
  def inTemplate: TemplateEntity
  def toRoot: List[Entity]
  def qualifiedName: String
  override def toString = qualifiedName
  def universe: Universe
}

/** A class, trait, object or package. A package is represented as an instance
  * of the `Package` subclass. A class, trait, object or package may be
  * directly an instance of `WeakTemplateEntity` if it is not ''documentable''
  * (that is, if there is no documentation page for it in the current site),
  * otherwise, it will be represented as an instance of the `TemplateEntity`
  * subclass. */
trait TemplateEntity extends Entity {
  def isPackage: Boolean
  def isRootPackage: Boolean
  def isTrait: Boolean
  def isClass: Boolean
  def isObject: Boolean
  def isDocTemplate: Boolean
  def selfType : Option[TypeEntity]
}
trait NoDocTemplate extends TemplateEntity

/** A member of a class, trait, object or package. */
trait MemberEntity extends Entity {
  def comment: Option[Comment]
  def inTemplate: DocTemplateEntity
  def toRoot: List[MemberEntity]
  def inDefinitionTemplates: List[TemplateEntity]
  def definitionName: String
  def visibility: Visibility
  def flags: List[Paragraph]
  def deprecation: Option[Body]
  def inheritedFrom: List[TemplateEntity]
  def resultType: TypeEntity
  def isDef: Boolean
  def isVal: Boolean
  def isLazyVal: Boolean
  def isVar: Boolean
  def isImplicit: Boolean
  def isAbstract: Boolean
  def isConstructor: Boolean
  def isAliasType: Boolean
  def isAbstractType: Boolean
  def isTemplate: Boolean
}

trait HigherKinded extends Entity {
  def typeParams: List[TypeParam]
}

/** A ''documentable'' class, trait or object (that is, a documentation page
  * will be generated for it in the current site). */
trait DocTemplateEntity extends TemplateEntity with MemberEntity {
  def toRoot: List[DocTemplateEntity]
  def inSource: Option[(io.AbstractFile, Int)]
  def sourceUrl: Option[java.net.URL]
  def parentType: Option[TypeEntity]
  def linearization: List[(TemplateEntity, TypeEntity)]
  def linearizationTemplates: List[TemplateEntity]
  def linearizationTypes: List[TypeEntity]
  def subClasses: List[DocTemplateEntity]
  def members: List[MemberEntity]
  def templates: List[DocTemplateEntity]
  def methods: List[Def]
  def values: List[Val]
  def abstractTypes: List[AbstractType]
  def aliasTypes: List[AliasType]
  def companion: Option[DocTemplateEntity]
  // temporary implementation: to be removed
  def findMember(str: String): Option[DocTemplateEntity] = {
    val root = toRoot.last
    val path = if (str.length > 0) str.split("\\.") else Array[String]()
    var i = 0;
    var found: DocTemplateEntity = root
    while(i < path.length && found != null) {
      found = found.members.find(_.name == path(i)) match {
        case Some(doc:DocTemplateEntity) => doc
        case _ => null
      }
      i += 1
    }
    Option(found)
  }
}

/** A ''documentable'' trait. */
trait Trait extends DocTemplateEntity with HigherKinded {
  def valueParams : List[List[ValueParam]]
}

/** A ''documentable'' class. */
trait Class extends Trait with HigherKinded {
  def primaryConstructor: Option[Constructor]
  def constructors: List[Constructor]
  def isCaseClass: Boolean
}

/** A ''documentable'' object. */
trait Object extends DocTemplateEntity

/** A package that contains at least one ''documentable'' class, trait,
  * object or package. */
trait Package extends Object {
  def inTemplate: Package
  def toRoot: List[Package]
  def packages: List[Package]
}

/** A package represent the root of Entities hierarchy */
trait RootPackage extends Package

trait NonTemplateMemberEntity extends MemberEntity {
  def isUseCase: Boolean
}

/** A method (`def`) of a ''documentable'' class, trait or object. */
trait Def extends NonTemplateMemberEntity with HigherKinded {
  def valueParams : List[List[ValueParam]]
}

trait Constructor extends NonTemplateMemberEntity {
  def isPrimary: Boolean
  def valueParams : List[List[ValueParam]]
}

/** A value (`val`), lazy val (`lazy val`) or variable (`var`) of a
  * ''documentable'' class, trait or object. */
trait Val extends NonTemplateMemberEntity

/** An abstract type of a ''documentable'' class, trait or object. */
trait AbstractType extends NonTemplateMemberEntity with HigherKinded {
  def lo: Option[TypeEntity]
  def hi: Option[TypeEntity]
}

/** An abstract type of a ''documentable'' class, trait or object. */
trait AliasType extends NonTemplateMemberEntity with HigherKinded {
  def alias: TypeEntity
}

trait ParameterEntity extends Entity {
  def isTypeParam: Boolean
  def isValueParam: Boolean
}

/** A type parameter to a class or trait or to a method. */
trait TypeParam extends ParameterEntity with HigherKinded {
  def variance: String
  def lo: Option[TypeEntity]
  def hi: Option[TypeEntity]
}

/** A value parameter to a constructor or to a method. */
trait ValueParam extends ParameterEntity {
  def resultType: TypeEntity
  def defaultValue: Option[TreeEntity]
  def isImplicit: Boolean
}

/** An type that represents visibility of members. */
sealed trait Visibility {
  def isProtected: Boolean = false
  def isPublic: Boolean = false
}

/** The visibility of `private[this]` members. */
case class PrivateInInstance() extends Visibility

/** The visibility of `protected[this]` members. */
case class ProtectedInInstance() extends Visibility {
  override def isProtected = true
}

/** The visibility of `private[owner]` members. An unqualified private members
  * is encoded with `owner` equal to the members's `inTemplate`. */
case class PrivateInTemplate(owner: TemplateEntity) extends Visibility

/** The visibility of `protected[owner]` members. An unqualified protected
  * members is encoded with `owner` equal to the members's `inTemplate`.
  * Note that whilst the member is visible in any template owned by `owner`,
  * it is only visible in subclasses of the member's `inTemplate`. */
case class ProtectedInTemplate(owner: TemplateEntity) extends Visibility {
  override def isProtected = true
}

/** The visibility of public members. */
case class Public() extends Visibility {
  override def isPublic = true
}
