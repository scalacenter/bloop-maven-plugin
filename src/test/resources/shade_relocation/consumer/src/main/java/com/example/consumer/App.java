package com.example.consumer;

import com.example.lib.Greeter;

public class App {
    public static void main(String[] args) {
        System.out.println(new Greeter().greet("world"));
    }
}
