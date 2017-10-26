/*
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package bug;

import bug.Named_1;
import java.io.Serializable;
import java.lang.annotation.Annotation;

public class NamedImpl_1 implements Named_1 {

    public Class<? extends Annotation> annotationType() {
        return null;
    }
}
