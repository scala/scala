package org.scalacheck;

import sbt.testing.*;

public class LazyFramework implements Framework {
    private final Framework framework;

    public LazyFramework() throws Exception {
        this.framework = (Framework)Class.forName("org.scalacheck.CustomScalaCheckFramework").newInstance();
    }

    public String name() { return framework.name(); }
    public Fingerprint[] fingerprints() { return framework.fingerprints(); }
    public Runner runner(String[] strings, String[] strings1, ClassLoader classLoader) {
        return new LazyRunner(framework, strings, strings1, classLoader);
    }
}

class LazyRunner implements Runner {
    private Runner _runner;
    private final Framework framework;
    private final String[] args, remoteArgs;
    private final ClassLoader classLoader;

    public LazyRunner(Framework framework, String[] args, String[] remoteArgs, ClassLoader classLoader) {
        this.framework = framework;
        this.args = args;
        this.remoteArgs = remoteArgs;
        this.classLoader = classLoader;
    }

    private Runner runner() {
        if(_runner == null) _runner = framework.runner(args, remoteArgs, classLoader);
        return _runner;
    }

    public Task[] tasks(TaskDef[] taskDefs) { return runner().tasks(taskDefs); }
    public String done() { if(_runner != null) return _runner.done(); else return "(no runner)"; }
    public String[] remoteArgs() { return remoteArgs; }
    public String[] args() { return args; }
}
