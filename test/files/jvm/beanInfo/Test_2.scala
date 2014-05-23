object Test extends App {
  val info = java.beans.Introspector.getBeanInfo(classOf[p.C])

  println("property descriptors")

  val pds = info.getPropertyDescriptors
  for (pd <- pds) {
    println(s"${pd.getName} -- ${pd.getPropertyType} -- ${pd.getReadMethod} -- ${pd.getWriteMethod}")
  }

  println("method descriptors")

  val mds = info.getMethodDescriptors
  for (md <- mds) {
    println(s"${md.getName} -- ${md.getMethod}")
  }
}
