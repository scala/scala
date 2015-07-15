/*
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package bug;

import bug.Named_2;
import java.io.Serializable;
import java.lang.annotation.Annotation;

public class NamedImpl_2 implements Named_2 {

    public Class<? extends Annotation> annotationType() {
        return null;
    }
}
