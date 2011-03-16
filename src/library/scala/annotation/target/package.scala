package scala.annotation

/**
 * When defining a field, the Scala compiler creates up to four accessors
 * for it: a getter, a setter, and if the field is annotated with
 * `@BeanProperty`, a bean getter and a bean setter.
 *
 * For instance in the following class definition
 *
 * {{{
 * class C(@myAnnot @BeanProperty var c: Int)
 * }}}
 *
 * there are six entities which can carry the annotation `@myAnnot`: the
 * constructor parameter, the generated field and the four accessors.
 *
 * By default, annotations on (`val`-, `var`- or plain) constructor parameters
 * end up on the parameter, not on any other entity. Annotations on fields
 * by default only end up on the field.
 *
 * The meta-annotations in package `scala.annotation.target` are used
 * to control where annotations on fields and class parameters are copied.
 * This is done by annotating either the annotation type or the annotation
 * class with one or several of the meta-annotations in this package.
 *
 * ==Annotating the annotation type==
 *
 * The target meta-annotations can be put on the annotation type when
 * instantiating the annotation. In the following example, the annotation
 * `@Id` will be added only to the bean getter `getX`.
 *
 * {{{
 * import javax.persistence.Id
 * class A {
 *   @(Id @beanGetter) @BeanProperty val x = 0
 * }
 * }}}
 *
 * In order to annotate the field as well, the meta-annotation `@field`
 * would need to be added.
 *
 * The syntax can be improved using a type alias:
 *
 * {{{
 * object ScalaJPA {
 *   type Id = javax.persistence.Id @beanGetter
 * }
 * import ScalaJPA.Id
 * class A {
 *   @Id @BeanProperty val x = 0
 * }
 * }}}
 *
 * ==Annotating the annotation class==
 *
 * For annotations defined in Scala, a default target can be specified
 * in the annotation class itself, for example
 *
 * {{{
 * @getter
 * class myAnnotation extends Annotation
 * }}}
 *
 * This only changes the default target for the annotation `myAnnotation`.
 * When instantiating the annotation, the target can still be specified
 * as described in the last section.
 */
package object target
