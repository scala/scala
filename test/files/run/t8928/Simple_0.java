package test;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface Simple_0 {
    byte _byte();
    char _char();
    short _short();
    int _int();
    long _long();
    float _float();
    double _double();
    String _string();
    Class<?> _class();

    short THREE = 3;
}