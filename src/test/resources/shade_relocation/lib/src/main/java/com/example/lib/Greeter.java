package com.example.lib;

import org.apache.commons.lang3.StringUtils;

public class Greeter {
    public String greet(String name) {
        // References a class that shade relocates into com.example.lib.shaded.lang3,
        // so the resulting bytecode only resolves against the shaded JAR.
        return "Hello, " + StringUtils.capitalize(name);
    }
}
