package scala.tools.nsc.interpreter

trait ClassBasedPaths {
  self: IMain =>
  override def transformPath(p: String): String = p replaceFirst("read", "read.INSTANCE") replaceAll("iwC", "iw")

  override def readRootPath(readPath: String) = getClassIfDefined(readPath)
}
