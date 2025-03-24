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

package scala.tools.nsc.profile;

import javax.management.ObjectName;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Method;

@SuppressWarnings("unused")
public abstract class ExtendedThreadMxBean implements ThreadMXBean {
    static final ExtendedThreadMxBean proxy;

    static {
        ExtendedThreadMxBean local;
        ThreadMXBean threadMx = ManagementFactory.getThreadMXBean();
        try {
            Class cls = Class.forName("com.sun.management.ThreadMXBean");
            if (cls.isInstance(threadMx)) {
                local = new SunThreadMxBean(threadMx);
            } else {
                local = new OtherThreadMxBean(threadMx);
            }
        } catch (ClassNotFoundException e) {
            local = new OtherThreadMxBean(threadMx);
        }
        proxy = local;
    }

    protected final ThreadMXBean underlying;

    protected ExtendedThreadMxBean(ThreadMXBean underlying) {
        this.underlying = underlying;
    }

    public abstract long[] getThreadUserTime(long[] longs) throws Exception;

    public abstract boolean isThreadAllocatedMemoryEnabled() throws Exception;

    public abstract void setThreadAllocatedMemoryEnabled(boolean b) throws Exception;

    public abstract long getThreadAllocatedBytes(long l) throws Exception;

    public abstract long[] getThreadAllocatedBytes(long[] longs) throws Exception;

    public abstract boolean isThreadAllocatedMemorySupported() throws Exception;

    public abstract long[] getThreadCpuTime(long[] longs) throws Exception;
    //common features from java.lang.management.ThreadMXBean

    @Override
    public int getThreadCount() {
        return underlying.getThreadCount();
    }

    @Override
    public int getPeakThreadCount() {
        return underlying.getPeakThreadCount();
    }

    @Override
    public long getTotalStartedThreadCount() {
        return underlying.getTotalStartedThreadCount();
    }

    @Override
    public int getDaemonThreadCount() {
        return underlying.getDaemonThreadCount();
    }

    @Override
    public long[] getAllThreadIds() {
        return underlying.getAllThreadIds();
    }

    @Override
    public ThreadInfo getThreadInfo(long id) {
        return underlying.getThreadInfo(id);
    }

    @Override
    public ThreadInfo[] getThreadInfo(long[] ids) {
        return underlying.getThreadInfo(ids);
    }

    @Override
    public ThreadInfo getThreadInfo(long id, int maxDepth) {
        return underlying.getThreadInfo(id, maxDepth);
    }

    @Override
    public ThreadInfo[] getThreadInfo(long[] ids, int maxDepth) {
        return underlying.getThreadInfo(ids, maxDepth);
    }

    @Override
    public boolean isThreadContentionMonitoringSupported() {
        return underlying.isThreadContentionMonitoringSupported();
    }

    @Override
    public boolean isThreadContentionMonitoringEnabled() {
        return underlying.isThreadContentionMonitoringEnabled();
    }

    @Override
    public void setThreadContentionMonitoringEnabled(boolean enable) {
        underlying.setThreadContentionMonitoringEnabled(enable);
    }

    @Override
    public long getCurrentThreadCpuTime() {
        return underlying.getCurrentThreadCpuTime();
    }

    @Override
    public long getCurrentThreadUserTime() {
        return underlying.getCurrentThreadUserTime();
    }

    @Override
    public long getThreadCpuTime(long id) {
        return underlying.getThreadCpuTime(id);
    }

    @Override
    public long getThreadUserTime(long id) {
        return underlying.getThreadUserTime(id);
    }

    @Override
    public boolean isThreadCpuTimeSupported() {
        return underlying.isThreadCpuTimeSupported();
    }

    @Override
    public boolean isCurrentThreadCpuTimeSupported() {
        return underlying.isCurrentThreadCpuTimeSupported();
    }

    @Override
    public boolean isThreadCpuTimeEnabled() {
        return underlying.isThreadCpuTimeEnabled();
    }

    @Override
    public void setThreadCpuTimeEnabled(boolean enable) {
        underlying.setThreadCpuTimeEnabled(enable);
    }

    @Override
    public long[] findMonitorDeadlockedThreads() {
        return underlying.findMonitorDeadlockedThreads();
    }

    @Override
    public void resetPeakThreadCount() {
        underlying.resetPeakThreadCount();
    }

    @Override
    public long[] findDeadlockedThreads() {
        return underlying.findDeadlockedThreads();
    }

    @Override
    public boolean isObjectMonitorUsageSupported() {
        return underlying.isObjectMonitorUsageSupported();
    }

    @Override
    public boolean isSynchronizerUsageSupported() {
        return underlying.isSynchronizerUsageSupported();
    }

    @Override
    public ThreadInfo[] getThreadInfo(long[] ids, boolean lockedMonitors, boolean lockedSynchronizers) {
        return underlying.getThreadInfo(ids, lockedMonitors, lockedSynchronizers);
    }

    @Override
    public ThreadInfo[] dumpAllThreads(boolean lockedMonitors, boolean lockedSynchronizers) {
        return underlying.dumpAllThreads(lockedMonitors, lockedSynchronizers);
    }

    @Override
    public ObjectName getObjectName() {
        return underlying.getObjectName();
    }
}

class OtherThreadMxBean extends ExtendedThreadMxBean {
    OtherThreadMxBean(ThreadMXBean underlying) {
        super(underlying);
    }

    @Override
    public long[] getThreadUserTime(long[] longs) throws Exception {
        return new long[0];
    }

    @Override
    public boolean isThreadAllocatedMemoryEnabled() throws Exception {
        return false;
    }

    @Override
    public void setThreadAllocatedMemoryEnabled(boolean b) throws Exception {

    }

    @Override
    public long getThreadAllocatedBytes(long l) throws Exception {
        return -1;
    }

    @Override
    public long[] getThreadAllocatedBytes(long[] longs) throws Exception {
        return new long[0];
    }

    @Override
    public boolean isThreadAllocatedMemorySupported() throws Exception {
        return false;
    }

    @Override
    public long[] getThreadCpuTime(long[] longs) throws Exception {
        return new long[0];
    }

}


class SunThreadMxBean extends ExtendedThreadMxBean {

    private final ThreadMXBean real;

    private final Method getThreadUserTimeMethod;
    private final Method isThreadAllocatedMemoryEnabledMethod;
    private final Method setThreadAllocatedMemoryEnabledMethod;
    private final Method getThreadAllocatedBytesMethod1;
    private final Method getThreadAllocatedBytesMethod2;
    private final Method isThreadAllocatedMemorySupportedMethod;
    private final Method getThreadCpuTimeMethod;


    public SunThreadMxBean(ThreadMXBean underlying) {
        super(underlying);
        this.real = underlying;
        try {
            Class<?> cls = Class.forName("com.sun.management.ThreadMXBean");
            getThreadUserTimeMethod = cls.getMethod("getThreadUserTime", long[].class);
            isThreadAllocatedMemoryEnabledMethod = cls.getMethod("isThreadAllocatedMemoryEnabled");
            setThreadAllocatedMemoryEnabledMethod = cls.getMethod("setThreadAllocatedMemoryEnabled", Boolean.TYPE);
            getThreadAllocatedBytesMethod1 = cls.getMethod("getThreadAllocatedBytes", Long.TYPE);
            getThreadAllocatedBytesMethod2 = cls.getMethod("getThreadAllocatedBytes", long[].class);
            isThreadAllocatedMemorySupportedMethod = cls.getMethod("isThreadAllocatedMemorySupported");
            getThreadCpuTimeMethod = cls.getMethod("getThreadCpuTime", long[].class);

            getThreadUserTimeMethod.setAccessible(true);
            isThreadAllocatedMemoryEnabledMethod.setAccessible(true);
            setThreadAllocatedMemoryEnabledMethod.setAccessible(true);
            getThreadAllocatedBytesMethod1.setAccessible(true);
            getThreadAllocatedBytesMethod2.setAccessible(true);
            isThreadAllocatedMemorySupportedMethod.setAccessible(true);
            getThreadCpuTimeMethod.setAccessible(true);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }


    public boolean isExtended() {
        return true;
    }

    public long[] getThreadUserTime(long[] longs) throws Exception {
        return (long[]) getThreadUserTimeMethod.invoke(real, longs);
    }

    public boolean isThreadAllocatedMemoryEnabled() throws Exception {
        return (boolean) isThreadAllocatedMemoryEnabledMethod.invoke(real);
    }

    public void setThreadAllocatedMemoryEnabled(boolean b) throws Exception {
        setThreadAllocatedMemoryEnabledMethod.invoke(real, b);
    }

    public long getThreadAllocatedBytes(long l) throws Exception {
        return (long) getThreadAllocatedBytesMethod1.invoke(real,l);
    }

    public long[] getThreadAllocatedBytes(long[] longs) throws Exception {
        return (long[]) getThreadAllocatedBytesMethod2.invoke(real, longs);
    }

    public boolean isThreadAllocatedMemorySupported() throws Exception {
        return (boolean) isThreadAllocatedMemorySupportedMethod.invoke(real);
    }

    public long[] getThreadCpuTime(long[] longs) throws Exception {
        return (long[]) getThreadCpuTimeMethod.invoke(real, longs);

    }
}
