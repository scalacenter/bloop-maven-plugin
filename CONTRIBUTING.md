# Contributing to `bloop-maven-plugin`

Thanks a lot for being willing to contribute! `bloop-maven-plugin` is, you
guessed it, a Maven plugin build with [Maven](https://maven.apache.org/).

## Compiling the project

To fully compile the project you can do:

```
./mvnw clean compile
```

## Installing the project locally

Since install will cause the tests to run, you'll want to use `-DskipTests` for
the first time you run it. This will ensure that it can install. Then following
this, you can both install and test since the tests actually use a
self-published version of the plugin.

```
./mvnw clean install -DskipTests
```

## Testing

Mind the note above, you'll want to have correctly ran an `install` _before_ you
try to test.

```
./mvnw clean test
```

## Releasing

In order to release a new version you'll want to head over to the [release
action](https://github.com/scalacenter/bloop-maven-plugin/actions/workflows/release.yml).
There you'll want to click on `Run workflow` which will show you a dropdown for
you to provide the version you'd like to release. During the release workflow it
will tag, update the pom file, release, update the pom file again, and then push
back to the repository. It should be fully automated apart from manually
triggering the release.
