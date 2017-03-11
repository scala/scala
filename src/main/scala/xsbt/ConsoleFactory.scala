package xsbtpamflet

import xsbti.Logger

class ConsoleFactory extends xsbti.ConsoleFactory {
  def createConsole(args: Array[String], bootClasspathString: String,
    classpathString: String, initialCommands: String, cleanupCommands: String,
    loader: ClassLoader, bindNames: Array[String], bindValues: Array[AnyRef],
    log: Logger): xsbti.ConsoleInterface =
    new ConsoleInterface(args, bootClasspathString, classpathString,
      initialCommands, cleanupCommands, loader, bindNames, bindValues, log)
}
