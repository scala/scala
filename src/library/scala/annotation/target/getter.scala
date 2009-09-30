/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation.target

/**
 * For every field of a class, the Scala compiler generates up to four
 * synthetic accessors: getter, setter, bean getter and bean setter.
 * The meta-annotations in package {{{scala.annotation.target}}} are
 * used to control to which of the above members the annotations on
 * the field are copied. By default, field annotations are only added
 * to the actual field, but not to any of the accessors. By annotating
 * the annotation type with one or several of the meta-annotations this
 * behavior can be changed.
 *
 * In the following example, the annotation {{{@Id}}} will be added
 * only to the bean getter {{{getX}}}. In order to annotate the field
 * as well, the meta-annotation {{{@field}}} would need to be added.
 *
 * {{{
 * import javax.persistence.Id
 * class A {
 *   @(Id @beanGetter) @BeanProperty val x = 0
 * }
 * }}}
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
 */
final class getter extends StaticAnnotation
