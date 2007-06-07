package scala.tools.nsc.plugins


class PluginLoadException(filename: String, cause: Exception)
extends Exception(cause)
