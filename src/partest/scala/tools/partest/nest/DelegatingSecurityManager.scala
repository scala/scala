/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest.nest

import java.io.FileDescriptor
import java.net.InetAddress
import java.security.Permission

@deprecated("JDK 17 deprecates SecurityManager", since="2.13.7")
class DelegatingSecurityManager(delegate: SecurityManager) extends SecurityManager {
  override def checkExit(status: Int): Unit = if (delegate ne null) delegate.checkExit(status)
  override def checkPermission(perm: Permission): Unit = if (delegate ne null) delegate.checkPermission(perm)
  override def checkPermission(perm: Permission, context: AnyRef): Unit = if (delegate ne null) delegate.checkPermission(perm, context)
  override def checkExec(cmd: String): Unit = if (delegate ne null) delegate.checkExec(cmd)
  override def checkWrite(file: String): Unit = if (delegate ne null) delegate.checkWrite(file)
  override def checkDelete(file: String): Unit = if (delegate ne null) delegate.checkDelete(file)
  override def checkRead(file: String): Unit = if (delegate ne null) delegate.checkRead(file)
  override def checkRead(file: String, context: scala.Any): Unit = if (delegate ne null) delegate.checkRead(file, context)
  override def checkPropertyAccess(key: String): Unit = if (delegate ne null) delegate.checkPropertyAccess(key)
  override def checkAccept(host: String, port: Int): Unit = if (delegate ne null) delegate.checkAccept(host, port)
  override def checkWrite(fd: FileDescriptor): Unit = if (delegate ne null) delegate.checkWrite(fd)
  override def checkPrintJobAccess(): Unit = if (delegate ne null) delegate.checkPrintJobAccess()
  override def checkMulticast(maddr: InetAddress): Unit = if (delegate ne null) delegate.checkMulticast(maddr)
  override def checkSetFactory(): Unit = if (delegate ne null) delegate.checkSetFactory()
  override def checkLink(lib: String): Unit = if (delegate ne null) delegate.checkLink(lib)
  override def checkSecurityAccess(target: String): Unit = if (delegate ne null) delegate.checkSecurityAccess(target)
  override def checkListen(port: Int): Unit = if (delegate ne null) delegate.checkListen(port)
  override def checkAccess(t: Thread): Unit = if (delegate ne null) delegate.checkAccess(t)
  override def checkAccess(g: ThreadGroup): Unit = if (delegate ne null) delegate.checkAccess(g)
  override def checkCreateClassLoader(): Unit = if (delegate ne null) delegate.checkCreateClassLoader()
  override def checkPackageDefinition(pkg: String): Unit = if (delegate ne null) delegate.checkPackageDefinition(pkg)
  override def checkConnect(host: String, port: Int): Unit = if (delegate ne null) delegate.checkConnect(host, port)
  override def checkConnect(host: String, port: Int, context: scala.Any): Unit = if (delegate ne null) delegate.checkConnect(host, port, context)
  override def checkPackageAccess(pkg: String): Unit = if (delegate ne null) delegate.checkPackageAccess(pkg)
  override def checkPropertiesAccess(): Unit = if (delegate ne null) delegate.checkPropertiesAccess()
  override def checkRead(fd: FileDescriptor): Unit = if (delegate ne null) delegate.checkRead(fd)
}
