/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author Manohar Jonnalagedda
 * @author Gilles Dubochet
 */

package scala.tools.nsc
package doc
package model

import scala.collection._
import comment._


/** An entity in a Scaladoc universe. Entities are declarations in the program and correspond to symbols in the
  * compiler. Entities model the following Scala concepts:
  *  - classes and traits;
  *  - objects and package;
  *  - constructors;
  *  - methods;
  *  - values, lazy values, and variables;
  *  - abstract type members and type aliases;
  *  - type and value parameters;
  *  - annotations. */
trait Entity {

  /** The name of the entity. Note that the name does not qualify this entity uniquely; use its `qualifiedName`
    * instead. */
  def name : String

  /** The qualified name of the entity. This is this entity's name preceded by the qualified name of the template
    * of which this entity is a member. The qualified name is unique to this entity. */
  def qualifiedName: String

  /** The template of which this entity is a member. */
  def inTemplate: TemplateEntity

  /** The list of entities such that each is a member of the entity that follows it; the first entity is always this
    * entity, the last the root package entity. */
  def toRoot: List[Entity]

  /** The qualified name of this entity. */
  override def toString = qualifiedName

  /** The Scaladoc universe of which this entity is a member. */
  def universe: Universe

  /** The annotations attached to this entity, if any. */
  def annotations: List[Annotation]

}

object Entity {
  private def isDeprecated(x: Entity) = x match {
    case x: MemberEntity  => x.deprecation.isDefined
    case _                => false
  }
  /** Ordering deprecated things last. */
  implicit lazy val EntityOrdering: Ordering[Entity] =
    Ordering[(Boolean, String)] on (x => (isDeprecated(x), x.name))
}

/** A template, which is either a class, trait, object or package. Depending on whether documentation is available
  * or not, the template will be modeled as a [scala.tools.nsc.doc.model.NoDocTemplate] or a
  * [scala.tools.nsc.doc.model.DocTemplateEntity]. */
trait TemplateEntity extends Entity {

  /** Whether this template is a package (including the root package). */
  def isPackage: Boolean

  /** Whether this template is the root package. */
  def isRootPackage: Boolean

  /** Whether this template is a trait. */
  def isTrait: Boolean

  /** Whether this template is a class. */
  def isClass: Boolean

  /** Whether this template is an object. */
  def isObject: Boolean

  /** Whether documentation is available for this template. */
  def isDocTemplate: Boolean

  /** Whether this template is a case class. */
  def isCaseClass: Boolean

  /** The self-type of this template, if it differs from the template type. */
  def selfType : Option[TypeEntity]

}


/** An entity that is a member of a template. All entities, including templates, are member of another entity
  * except for parameters and annotations. Note that all members of a template are modelled, including those that are
  * inherited and not declared locally. */
trait MemberEntity extends Entity {

  /** The comment attached to this member, if any. */
  def comment: Option[Comment]

  /** The template of which this entity is a member. */
  def inTemplate: DocTemplateEntity

  /** The list of entities such that each is a member of the entity that follows it; the first entity is always this
    * member, the last the root package entity. */
  def toRoot: List[MemberEntity]

  /** The templates in which this member has been declared. The first element of the list is the template that contains
    * the currently active declaration of this member, subsequent elements are declarations that have been overriden. If
    * the first element is equal to `inTemplate`, the member is declared locally, if not, it has been inherited. All
    * elements of this list are in the linearization of `inTemplate`. */
  def inDefinitionTemplates: List[TemplateEntity]

  /** The qualified name of the member in its currently active declaration template. */
  def definitionName: String

  /** The visibility of this member. Note that members with restricted visibility may not be modeled in some
    * universes. */
  def visibility: Visibility

  /** The flags that have been set for this entity. The following flags are supported: `implicit`, `sealed`, `abstract`,
    * and `final`. */
  def flags: List[Paragraph]

  /** Some deprecation message if this member is deprecated, or none otherwise. */
  def deprecation: Option[Body]

  /** Some migration warning if this member has a migration annotation, or none otherwise. */
  def migration: Option[Body]

  @deprecated("Use `inDefinitionTemplates` instead", "2.9.0")
  def inheritedFrom: List[TemplateEntity]

  /** For members representing values: the type of the value returned by this member; for members
    * representing types: the type itself. */
  def resultType: TypeEntity

  /** Whether this member is a method. */
  def isDef: Boolean

  /** Whether this member is a value (this excludes lazy values). */
  def isVal: Boolean

  /** Whether this member is a lazy value. */
  def isLazyVal: Boolean

  /** Whether this member is a variable. */
  def isVar: Boolean

  /** Whether this member is a constructor. */
  def isConstructor: Boolean

  /** Whether this member is an alias type. */
  def isAliasType: Boolean

  /** Whether this member is an abstract type. */
  def isAbstractType: Boolean

  /** Whether this member is a template. */
  def isTemplate: Boolean

  /** Whether this member is implicit.  */
  def isImplicit: Boolean

  /** Whether this member is abstract. */
  def isAbstract: Boolean

}
object MemberEntity {
  // Oh contravariance, contravariance, wherefore art thou contravariance?
  // Note: the above works for both the commonly misunderstood meaning of the line and the real one.
  implicit lazy val MemberEntityOrdering: Ordering[MemberEntity] = Entity.EntityOrdering on (x => x)
}

/** An entity that is parameterized by types */
trait HigherKinded extends Entity {

  /** The type parameters of this entity. */
  def typeParams: List[TypeParam]

}


/** A template (class, trait, object or package) which is referenced in the universe, but for which no further
  * documentation is available. Only templates for which a source file is given are documented by Scaladoc. */
trait NoDocTemplate extends TemplateEntity


/** A template (class, trait, object or package) for which documentation is available. Only templates for which
  * a source file is given are documented by Scaladoc. */
trait DocTemplateEntity extends TemplateEntity with MemberEntity {

  /** The list of templates such that each is a member of the template that follows it; the first template is always
    * this template, the last the root package entity. */
  def toRoot: List[DocTemplateEntity]

  /** The source file in which the current template is defined and the line where the definition starts, if they exist.
    * A source file exists for all templates, except for those that are generated synthetically by Scaladoc. */
  def inSource: Option[(io.AbstractFile, Int)]

  /** An HTTP address at which the source of this template is available, if it is available. An address is available
    * only if the `docsourceurl` setting has been set. */
  def sourceUrl: Option[java.net.URL]

  /** The direct super-type of this template. */
  def parentType: Option[TypeEntity]

  @deprecated("Use `linearizationTemplates` and `linearizationTypes` instead", "2.9.0")
  def linearization: List[(TemplateEntity, TypeEntity)]

  /** All class, trait and object templates which are part of this template's linearization, in lineratization order.
    * This template's linearization contains all of its direct and indirect super-classes and super-traits. */
  def linearizationTemplates: List[TemplateEntity]

  /** All instantiated types which are part of this template's linearization, in lineratization order.
    * This template's linearization contains all of its direct and indirect super-types. */
  def linearizationTypes: List[TypeEntity]

  /**All class, trait and object templates for which this template is a direct or indirect super-class or super-trait.
   * Only templates for which documentation is available in the universe (`DocTemplateEntity`) are listed. */
  def subClasses: List[DocTemplateEntity]

  /** All members of this template. If this template is a package, only templates for which documentation is available
    * in the universe (`DocTemplateEntity`) are listed. */
  def members: List[MemberEntity]

  /** All templates that are members of this template. If this template is a package, only templates for which
    * documentation is available  in the universe (`DocTemplateEntity`) are listed. */
  def templates: List[DocTemplateEntity]

  /** All methods that are members of this template. */
  def methods: List[Def]

  /** All values, lazy values and variables that are members of this template. */
  def values: List[Val]

  /** All abstract types that are members of this template. */
  def abstractTypes: List[AbstractType]

  /** All type aliases that are members of this template. */
  def aliasTypes: List[AliasType]

  /** The companion of this template, or none. If a class and an object are defined as a pair of the same name, the
    * other entity of the pair is the companion. */
  def companion: Option[DocTemplateEntity]

}


/** A trait template. */
trait Trait extends DocTemplateEntity with HigherKinded


/** A class template. */
trait Class extends Trait with HigherKinded {

  /** The primary constructor of this class, if it has been defined. */
  def primaryConstructor: Option[Constructor]

  /** All constructors of this class, including the primary constructor. */
  def constructors: List[Constructor]

  /** The value parameters of this case class, or an empty list if this class is not a case class. As case class value
    * parameters cannot be curried, the outer list has exactly one element. */
  def valueParams: List[List[ValueParam]]

}


/** An object template. */
trait Object extends DocTemplateEntity


/** A package template. A package is in the universe if it is declared as a package object, or if it
  * contains at least one template. */
trait Package extends Object {

  /** The package of which this package is a member. */
  def inTemplate: Package

  /** The package such that each is a member of the package that follows it; the first package is always this
    * package, the last the root package. */
  def toRoot: List[Package]

  /** All packages that are member of this package. */
  def packages: List[Package]
}


/** The root package, which contains directly or indirectly all members in the universe. A universe
  * contains exactly one root package. */
trait RootPackage extends Package


/** A non-template member (method, value, lazy value, variable, constructor, alias type, and abstract type). */
trait NonTemplateMemberEntity extends MemberEntity {

  /** Whether this member is a use case. A use case is a member which does not exist in the documented code.
    * It corresponds to a real member, and provides a simplified, yet compatible signature for that member. */
  def isUseCase: Boolean

  /** If this symbol is a use case, the useCaseOf will contain the member it was derived from, containing the full
    * signature and the complete parameter descriptions. */
  def useCaseOf: Option[MemberEntity]

  /** Whether this member is a bridge member. A bridge member does only exist for binary compatibility reasons
    * and should not appear in ScalaDoc. */
  def isBridge: Boolean

}


/** A method (`def`) of a template. */
trait Def extends NonTemplateMemberEntity with HigherKinded {

  /** The value parameters of this method. Each parameter block of a curried method is an element of the list.
    * Each parameter block is a list of value parameters. */
  def valueParams : List[List[ValueParam]]

}


/** A constructor of a class. */
trait Constructor extends NonTemplateMemberEntity {

  /** Whether this is the primary constructor of a class. The primary constructor is defined syntactically as part of
    * the declaration of the class. */
  def isPrimary: Boolean

  /** The value parameters of this constructor. As constructors cannot be curried, the outer list has exactly one
    * element. */
  def valueParams : List[List[ValueParam]]

}


/** A value (`val`), lazy val (`lazy val`) or variable (`var`) of a template. */
trait Val extends NonTemplateMemberEntity


/** An abstract type member of a template. */
trait AbstractType extends NonTemplateMemberEntity with HigherKinded {

  /** The lower bound for this abstract type, if it has been defined. */
  def lo: Option[TypeEntity]

  /** The upper bound for this abstract type, if it has been defined. */
  def hi: Option[TypeEntity]

}


/** An type alias of a template. */
trait AliasType extends NonTemplateMemberEntity with HigherKinded {

  /** The type aliased by this type alias. */
  def alias: TypeEntity

}


/** A parameter to an entity. */
trait ParameterEntity extends Entity {

  /** Whether this parameter is a type parameter. */
  def isTypeParam: Boolean

  /** Whether this parameter is a value parameter. */
  def isValueParam: Boolean

}


/** A type parameter to a class, trait, or method. */
trait TypeParam extends ParameterEntity with HigherKinded {

  /** The variance of this type parameter. Valid values are "+", "-", and the empty string. */
  def variance: String

  /** The lower bound for this type parameter, if it has been defined. */
  def lo: Option[TypeEntity]

  /** The upper bound for this type parameter, if it has been defined. */
  def hi: Option[TypeEntity]

}


/** A value parameter to a constructor or method. */
trait ValueParam extends ParameterEntity {

  /** The type of this value parameter. */
  def resultType: TypeEntity

  /** The devault value of this value parameter, if it has been defined. */
  def defaultValue: Option[TreeEntity]

  /** Whether this value parameter is implicit. */
  def isImplicit: Boolean

}


/** An annotation to an entity. */
trait Annotation extends Entity {

  /** The class of this annotation. */
  def annotationClass: TemplateEntity

  /** The arguments passed to the constructor of the annotation class. */
  def arguments: List[ValueArgument]

}
