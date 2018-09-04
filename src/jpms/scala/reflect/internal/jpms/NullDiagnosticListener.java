package scala.reflect.internal.jpms;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;

class NullDiagnosticListener<S> implements DiagnosticListener<S> {
    @Override
    public void report(Diagnostic<? extends S> diagnostic) {
    }
}
