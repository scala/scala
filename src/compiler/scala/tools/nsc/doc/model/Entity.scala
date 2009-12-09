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
}


/** A class, trait, object or package. A package is represented as an instance of the `Package` subclass. A class,
  * trait, object or package may be directly an instance of `WeakTemplateEntity` if it is not ''documentable'' (that
  * is, if there is no documentation page for it in the current site), otherwise, it will be represented as an instance
  * of the `TemplateEntity` subclass. */
trait TemplateEntity extends Entity {
  def isPackage: Boolean
  def isRootPackage: Boolean
  def isTrait: Boolean
  def isClass: Boolean
  def isObject: Boolean
  def isDocTemplate: Boolean
}
trait NoDocTemplate extends TemplateEntity

/** A member of a class, trait, object or package. */
trait MemberEntity extends Entity {
  def comment: Option[Comment]
  def inTemplate: DocTemplateEntity
  def toRoot: List[MemberEntity]
  def inDefinitionTemplates: List[TemplateEntity]
  def definitionName: String
  def visibility: Option[Paragraph]
  def flags: List[Paragraph]
  def inheritedFrom: List[TemplateEntity]
  def isDeprecated: Boolean
  def resultType: TypeEntity
  def isDef: Boolean
  def isVal: Boolean
  def isVar: Boolean
  def isConstructor: Boolean
  def isAliasType: Boolean
  def isAbstractType: Boolean
  def isTemplate: Boolean
}

/** A ''documentable'' class, trait or object (that is, a documentation page will be generated for it in the current
  * site). */
trait DocTemplateEntity extends TemplateEntity with MemberEntity {
  def toRoot: List[DocTemplateEntity]
  def inSource: Option[(io.AbstractFile, Int)]
  def typeParams: List[TypeParam]
  def parentType: Option[TypeEntity]
  def linearization: List[TemplateEntity]
  def subClasses: List[DocTemplateEntity]
  def members: List[MemberEntity]
  def templates: List[DocTemplateEntity]
  def methods: List[Def]
  def values: List[Val]
  def abstractTypes: List[AbstractType]
  def aliasTypes: List[AliasType]
}

/** A ''documentable'' trait. */
trait Trait extends DocTemplateEntity {
  def valueParams : List[List[ValueParam]]
}

/** A ''documentable'' class. */
trait Class extends Trait {
  def primaryConstructor: Option[Constructor]
  def constructors: List[Constructor]
  def isCaseClass: Boolean
}

/** A ''documentable'' object. */
trait Object extends DocTemplateEntity

/** A package that contains at least one ''documentable'' class, trait, object or package. */
trait Package extends Object {
  def inTemplate: Package
  def toRoot: List[Package]
  def packages: List[Package]
}

trait NonTemplateMemberEntity extends MemberEntity

/** A method (`def`) of a ''documentable'' class, trait or object. */
trait Def extends NonTemplateMemberEntity {
  def typeParams: List[TypeParam]
  def valueParams : List[List[ValueParam]]
}

trait Constructor extends NonTemplateMemberEntity {
  def isPrimary: Boolean
  def valueParams : List[List[ValueParam]]
}

/** A value (`val`) or variable (`var`) of a ''documentable'' class, trait or object. */
trait Val extends NonTemplateMemberEntity

/** An abstract type of a ''documentable'' class, trait or object. */
trait AbstractType extends NonTemplateMemberEntity {
  // TODO: typeParams
  def lo: Option[TypeEntity]
  def hi: Option[TypeEntity]
}

/** An abstract type of a ''documentable'' class, trait or object. */
trait AliasType extends NonTemplateMemberEntity {
  // TODO: typeParams
  def alias: TypeEntity
}

trait ParameterEntity extends Entity {
  def inTemplate: DocTemplateEntity
  def isTypeParam: Boolean
  def isValueParam: Boolean
}

/** A type parameter to a class or trait or to a method. */
trait TypeParam extends ParameterEntity {
  def variance: String
  def lo: Option[TypeEntity]
  def hi: Option[TypeEntity]
}

/** A value parameter to a constructor or to a method. */
trait ValueParam extends ParameterEntity {
  def resultType : TypeEntity
}
