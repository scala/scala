package scala.reflect.internal.jpms;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

final public class StoreDiagnosticListener<S> implements DiagnosticListener<S> {
    private List<Diagnostic<? extends S>> diagnostics = new ArrayList<>();

    @Override
    public void report(Diagnostic<? extends S> diagnostic) {
        diagnostics.add(diagnostic);
    }

    public List<Diagnostic<? extends S>> getDiagnostics() {
        return diagnostics;
    }

    public List<Diagnostic<? extends S>> getErrors() {
        return diagnostics.stream().filter(diagnostic -> diagnostic.getKind().equals(Diagnostic.Kind.ERROR)).collect(Collectors.toList());
    }
}
