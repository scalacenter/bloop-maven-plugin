# bloop-maven-plugin

<div align="center">
<img alt="release" src="https://img.shields.io/github/release/scalacenter/bloop-maven-plugin.svg?color=green&logo=github&style=flat-square">
<img alt="license" src="https://img.shields.io/github/license/scalacenter/bloop-maven-plugin.svg?color=blue&style=flat-square" />
<img alt="discord" src="https://img.shields.io/discord/632150470000902164?label=%23bloop%20in%20the%20Scala%20Discord&style=flat-square" />
</div>

This repository holds the Maven Plugin for
[Bloop](https://scalacenter.github.io/bloop/).

**NOTE**: This project was previously published as
`ch.epfl.scala:maven-bloop_2.13`, however it's now published as
`ch.epfl.scala:bloop-maven-plugin`. The usage remains the same.

## Getting Started

- To get started please refer to the docs [here](https://scalacenter.github.io/bloop/docs/build-tools/maven).
- If you're looking for the main Bloop codebase, head over [here](https://github.com/scalacenter/bloop).
- If you're interested in contributing, check out our [CONTRIBUTING.md](./CONTRIBUTING.md).

## Known limitations

### `--add-exports` / `--add-opens` and JDK-internal types

If a Java class extends or otherwise hard-depends on a JDK-internal type that is
only reachable via `--add-exports`/`--add-opens` (for example a class extending
`jdk.javadoc.internal.tool.DocEnvImpl`), Bloop can fail to compile it even though
Maven succeeds, with an error such as:

```
java.lang.IllegalAccessError: superclass access check failed: ... cannot access
class jdk.javadoc.internal.tool.DocEnvImpl (in module jdk.javadoc) because module
jdk.javadoc does not export jdk.javadoc.internal.tool to unnamed module
```

This plugin **does** forward those options correctly: any `--add-exports` you set
in `maven-compiler-plugin`'s `<compilerArgs>` ends up verbatim in the generated
`java.options`, so `javac` itself compiles fine. The failure happens *afterwards*,
when Zinc (inside the Bloop **server** JVM) reflectively loads the freshly compiled
classes to extract their API — that JVM was not launched with the `--add-exports`
flag, so the class load is rejected. This is outside the reach of a per-project
config file and is tracked upstream in
[sbt/zinc#837](https://github.com/sbt/zinc/issues/837).

**Workaround:** start the Bloop server with the same flag, e.g.

```bash
export BLOOP_JAVA_OPTS="--add-exports jdk.javadoc/jdk.javadoc.internal.tool=ALL-UNNAMED"
```

(or set the equivalent `bloop.java-opts` property). This grants the export to the
JVM that runs Zinc. Note it is global to the server, not per-project.
