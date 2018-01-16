package j;

import java.lang.annotation.*;

public class TableColumnImpl implements TableColumn {
    private final int width;

    public TableColumnImpl(int width) { this.width = width; }

    @Override public int width() { return this.width; }

    @Override public Class<? extends Annotation> annotationType() {
        return TableColumn.class;
    }
}
