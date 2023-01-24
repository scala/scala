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

package scala.reflect.internal

package object util {

  // An allocation-avoiding reusable instance of the so-common List(Nil).
  val ListOfNil: List[List[Nothing]] = Nil :: Nil
  val SomeOfNil: Option[List[Nothing]] = Some(Nil)

  // Shorten a name like Symbols$FooSymbol to FooSymbol.
  private def shortenName(name: String): String = {
    if (name == "") return ""
    val segments = (name split '$').toList
    val last     = segments.last

    if (last.length == 0)
      segments takeRight 2 mkString "$"
    else
      last
  }

  def shortClassOfInstance(x: AnyRef): String = shortClass(x.getClass)
  def shortClass(clazz: Class[_]): String = {
    val name: String = (clazz.getName split '.').last
    def isModule     = name endsWith "$"                        // object
    def isAnon       = (name split '$').last forall (_.isDigit) // anonymous class

    if (isModule)
      (name split '$' filterNot (_ == "")).last + "$"
    else if (isAnon)
      clazz.getInterfaces.toList.prepended(clazz.getSuperclass).map(shortClass).mkString(" with ")
    else
      shortenName(name)
  }
  /**
   * Adds the `sm` String interpolator to a [[scala.StringContext]].
   */
  implicit class StringContextStripMarginOps(val stringContext: StringContext) extends StripMarginInterpolator
}
