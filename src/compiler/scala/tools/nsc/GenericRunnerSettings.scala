/* NSC -- new Scala compiler
 * Copyright 2006 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

import java.lang.System.getProperties
import scala.collection.mutable.Queue

class GenericRunnerSettings(error: String => Unit)
extends Settings(error) {
  val howtorun =
    ChoiceSetting(
      "-howtorun",
      "how to run the specified code",
      List("guess", "object", "script"),
      "guess")

  val savecompiled =
    BooleanSetting(
        "-savecompiled",
        "save the compiled script (assumes the code is a script)")

  val nocompdaemon =
    BooleanSetting(
        "-nocompdaemon",
        "do not use the fsc compilation daemon")

  /* For some reason, "object defines extends Setting(...)"
     does not work here.  The object is present but the setting
     is not added to allsettings.  Thus,
  */
  class DefinesSetting
  extends Setting("-D<prop>", "set a Java property")
  {
    private val props = new Queue[(String, String)]

    def value = props.toList

    def tryToSet(args: List[String]): List[String] = {
      args match {
        case arg0::rest
        if arg0.startsWith("-D") =>
        {
          val stripD = arg0.substring(2)
          val eqidx = stripD.indexOf('=')
          val addition =
            if(eqidx < 0)
              (stripD, "")
            else
              (stripD.substring(0, eqidx), stripD.substring(eqidx+1))
          props += addition
          rest
        }

        case _ => args
      }
    }

    /** Apply the specified properties to the current JVM */
    def applyToCurrentJVM = {
      val systemProps = getProperties
      for(val (key, value) <- props.toList)
        systemProps.setProperty(key, value)
    }

    def unparse: List[String] =
      (props.toList.foldLeft[List[String]]
        (Nil)
        ((args, prop) =>
         ("-D" + prop._1 + "=" + prop._2) :: args))
  }

  val defines = new DefinesSetting
}
