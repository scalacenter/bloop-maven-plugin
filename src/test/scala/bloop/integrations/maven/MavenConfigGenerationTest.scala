package bloop.integrations.maven

import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption

import scala.util.Try
import scala.util.control.NonFatal

import bloop.config.Config
import bloop.config.Config.Platform
import bloop.config.Tag

import org.junit.Assert._
import org.junit.Test
import java.util.Properties
import java.io.InputStream

class MavenConfigGenerationTest extends BaseConfigSuite {

  @Test
  def basicScala3() = {
    check("basic_scala3/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isDefined)
      assertEquals("3.0.0", configFile.project.`scala`.get.version)
      assertEquals("org.scala-lang", configFile.project.`scala`.get.organization)
      assert(configFile.project.`scala`.get.jars.exists(_.toString.contains("scala3-compiler_3")))
      assert(hasCompileClasspathEntryName(configFile, "scala3-library_3"))
      assert(hasCompileClasspathEntryName(configFile, "scala-library"))

      val idxDottyLib = idxOfClasspathEntryName(configFile, "scala3-library_3")
      val idxScalaLib = idxOfClasspathEntryName(configFile, "scala-library")

      assert(idxDottyLib < idxScalaLib)

      assert(hasTag(configFile, Tag.Library))

      assertNoConfigsHaveAnyJars(List(configFile), List(s"$projectName", s"$projectName-test"))
      assertAllConfigsMatchJarNames(List(configFile), List("scala3-library_3"))
    }
  }

  @Test
  def basicScala() = {
    check("basic_scala/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isDefined)
      assertEquals("2.13.6", configFile.project.`scala`.get.version)
      assertEquals("org.scala-lang", configFile.project.`scala`.get.organization)
      assert(
        !configFile.project.`scala`.get.jars.exists(_.toString.contains("scala3-compiler_3")),
        "No Scala 3 jar should be present."
      )
      assert(!hasCompileClasspathEntryName(configFile, "scala3-library_3"))
      assert(hasCompileClasspathEntryName(configFile, "scala-library"))

      assert(hasTag(configFile, Tag.Library))

      assertNoConfigsHaveAnyJars(List(configFile), List(s"$projectName", s"$projectName-test"))
      assertAllConfigsMatchJarNames(List(configFile), List("scala-library", "munit"))
    }
  }

  @Test
  def launcher() = {
    check("launcher/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isDefined)
      assertEquals("2.13.12", configFile.project.`scala`.get.version)
      assertEquals("org.scala-lang", configFile.project.`scala`.get.organization)
      assert(
        !configFile.project.`scala`.get.jars.exists(_.toString.contains("scala3-compiler_3")),
        "No Scala 3 jar should be present."
      )
      assert(!hasCompileClasspathEntryName(configFile, "scala3-library_3"))
      assert(hasCompileClasspathEntryName(configFile, "scala-library"))

      assert(hasTag(configFile, Tag.Library))

      assertNoConfigsHaveAnyJars(List(configFile), List(s"$projectName", s"$projectName-test"))
      assertAllConfigsMatchJarNames(List(configFile), List("scala-library", "munit"))

      configFile.project.platform match {
        case Some(jvm: Platform.Jvm) =>
          assertEquals(jvm.config.options, List("--jvm-arg a", "--jvm-arg b"))
          assertEquals(jvm.mainClass, Some("com.example.Main"))
        case _ => fail("Missing platform")
      }
    }
  }

  @Test
  def multiProject() = {
    check(
      "multi_scala/pom.xml",
      submodules = List("multi_scala/module1/pom.xml", "multi_scala/module2/pom.xml")
    ) {
      case (configFile, projectName, List(module1, module2)) =>
        assert(configFile.project.`scala`.isEmpty)
        assert(module1.project.`scala`.isEmpty)
        assert(module2.project.`scala`.isDefined)

        assertEquals("2.13.6", module2.project.`scala`.get.version)
        assertEquals("org.scala-lang", module2.project.`scala`.get.organization)
        assert(
          !module2.project.`scala`.get.jars.exists(_.toString.contains("scala3-compiler_3")),
          "No Scala 3 jar should be present."
        )
        assert(hasCompileClasspathEntryName(module2, "scala-library"))

        assert(hasTag(module1, Tag.Library))
        assert(hasTag(module2, Tag.Library))

        assertNoConfigsHaveAnyJars(List(configFile), List(s"$projectName", s"$projectName-test"))
        assertNoConfigsHaveAnyJars(List(module1), List("module1", "module1-test"))
        assertNoConfigsHaveAnyJars(List(module2), List("module2", "module2-test"))

        assertAllConfigsMatchJarNames(List(module1), List("scala-library", "munit"))
      case _ =>
        assert(false, "Multi project should have two submodules")
    }
  }

  @Test
  def multiDependency() = {
    check(
      "multi_dependency/pom.xml",
      submodules = List("multi_dependency/module1/pom.xml", "multi_dependency/module2/pom.xml")
    ) {
      case (configFile, projectName, List(module1, module2)) =>
        assert(module1.project.`scala`.isDefined)
        assert(module2.project.`scala`.isDefined)
        assert(module1.project.resolution.nonEmpty)
        assert(module2.project.resolution.nonEmpty)

        val resolutionModules1 = module1.project.resolution.get.modules
        assert(resolutionModules1.nonEmpty)
        assert(resolutionModules1.forall(_.artifacts.exists(_.classifier == Some("sources"))))
        assert(resolutionModules1.forall(_.artifacts.exists(_.classifier == None)))

        // check for munit, direct dependency
        val munitModule1 = resolutionModules1.find(_.name == "munit_2.13")
        assert(munitModule1.exists { m =>
          m.artifacts.exists(_.path.toString().contains("munit_2.13-0.7.26-sources.jar"))
          m.artifacts.exists(_.path.toString().contains("munit_2.13-0.7.26.jar"))
        })

        val resolutionModules2 = module2.project.resolution.get.modules
        assert(resolutionModules2.nonEmpty)
        assert(resolutionModules2.forall(_.artifacts.exists(_.classifier == Some("sources"))))
        assert(resolutionModules2.forall(_.artifacts.exists(_.classifier == None)))

        // check for munit, direct dependency
        val munitModule2 = resolutionModules2.find(_.name == "munit_2.13")
        assert(munitModule2.exists { m =>
          m.artifacts.exists(_.path.toString().contains("munit_2.13-0.7.26-sources.jar"))
          m.artifacts.exists(_.path.toString().contains("munit_2.13-0.7.26.jar"))
        })

        // junit transitive dependency
        val junitModule = resolutionModules2.find(_.name == "junit")
        assert(junitModule.exists { m =>
          m.artifacts.exists(_.path.toString().contains("junit-4.13.1-sources.jar"))
          m.artifacts.exists(_.path.toString().contains("junit-4.13.1.jar"))
        })

        // scaltags in dependend module
        val scalatagsModule = resolutionModules2.find(_.name == "scalatags_2.13")
        assert(scalatagsModule.exists { m =>
          m.artifacts.exists(_.path.toString().contains("scalatags_2.13-0.8.2-sources.jar"))
          m.artifacts.exists(_.path.toString().contains("scalatags_2.13-0.8.2.jar"))
        })

      case _ =>
        assert(false, "Multi project should have two submodules")
    }
  }

  @Test
  def shadeRelocation() = {
    // `lib` is built by maven-shade-plugin: its relocated classes live only inside lib-0.1.jar,
    // never in lib/target/classes. The dependent `consumer` must therefore see the shaded JAR on
    // its classpath, not the (empty) output directory. Package first so the shaded JAR exists.
    check(
      "shade_relocation/pom.xml",
      submodules = List("shade_relocation/lib/pom.xml", "shade_relocation/consumer/pom.xml"),
      prePackage = true
    ) {
      case (configFile, projectName, List(lib, consumer)) =>
        assert(
          hasCompileClasspathEntryName(consumer, "lib-0.1.jar"),
          s"consumer should depend on the shaded JAR lib-0.1.jar; classpath=${consumer.project.classpath}"
        )
        val staleOutputDir = "lib" + File.separator + "target" + File.separator + "classes"
        assert(
          !hasCompileClasspathEntryName(consumer, staleOutputDir),
          s"consumer must NOT link the stale $staleOutputDir; classpath=${consumer.project.classpath}"
        )
        // The shaded module must NOT remain a bloop project dependency: a project dependency
        // contributes its classesDir (lib/target/classes) to the effective classpath, which would
        // re-introduce the empty directory through the dependency graph.
        assert(
          !consumer.project.dependencies.contains("lib"),
          s"consumer must not keep `lib` as a project dependency; deps=${consumer.project.dependencies}"
        )
      case _ =>
        assert(false, "shade_relocation should have lib and consumer submodules")
    }
  }

  @Test
  def shadeRelocationCleanFallback() = {
    // Without a prior `package`, the shaded JAR does not exist yet (shade binds to package, after
    // bloopInstall's generate-resources). This locks in the documented warn+fallback behavior:
    // the plugin must NOT wire a (stale) JAR, and instead leaves `lib` as a normal project
    // dependency with its target/classes on the classpath, exactly as before the shade handling.
    check(
      "shade_relocation/pom.xml",
      submodules = List("shade_relocation/lib/pom.xml", "shade_relocation/consumer/pom.xml"),
      prePackage = false
    ) {
      case (configFile, projectName, List(lib, consumer)) =>
        val staleOutputDir = "lib" + File.separator + "target" + File.separator + "classes"
        assert(
          hasCompileClasspathEntryName(consumer, staleOutputDir),
          s"clean export should fall back to $staleOutputDir; classpath=${consumer.project.classpath}"
        )
        assert(
          !hasCompileClasspathEntryName(consumer, "lib-0.1.jar"),
          s"clean export must NOT wire a shaded JAR that was not built; classpath=${consumer.project.classpath}"
        )
        assert(
          consumer.project.dependencies.contains("lib"),
          s"clean export should keep `lib` as a project dependency; deps=${consumer.project.dependencies}"
        )
      case _ =>
        assert(false, "shade_relocation should have lib and consumer submodules")
    }
  }

  @Test
  def noLibrary() = {
    check("no_library/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isDefined)
      assertEquals("2.13.6", configFile.project.`scala`.get.version)
      assertEquals("org.scala-lang", configFile.project.`scala`.get.organization)
      assert(configFile.project.`scala`.get.jars.exists(_.toString.contains("scala-compiler")))
      assert(hasCompileClasspathEntryName(configFile, "scala-library"))
      assert(hasTag(configFile, Tag.Library))
      assertNoConfigsHaveAnyJars(List(configFile), List(s"$projectName", s"$projectName-test"))

      val resolutionModules = configFile.project.resolution.get.modules
      val scalaLibraryModule = resolutionModules.find(_.name == "scala-library")
      assert(scalaLibraryModule.exists { m =>
        m.artifacts.exists(_.path.toString().contains("scala-library-2.13.6-sources.jar"))
        m.artifacts.exists(_.path.toString().contains("scala-library-2.13.6.jar"))
      })
    }
  }

  // See https://github.com/scalacenter/bloop-maven-plugin/issues/26 and the upstream
  // sbt/zinc#837. The plugin's only responsibility for `--add-exports` is to forward it
  // verbatim from maven-compiler-plugin's <compilerArgs> into the bloop config's
  // `java.options`; the IllegalAccessError reported in #26 happens later, inside zinc's
  // API extraction in the bloop server JVM, which is outside this plugin's reach.
  @Test
  def issue26() = {
    check("issue_26/pom.xml") { (configFile, _, subprojects) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isEmpty, "issue_26 is a Java-only module")
      val opts = configFile.project.java.get.options
      assert(
        opts.contains("--add-exports"),
        s"java.options should forward --add-exports, was: $opts"
      )
      assert(
        opts.contains("jdk.javadoc/jdk.javadoc.internal.tool=ALL-UNNAMED"),
        s"java.options should forward the --add-exports value, was: $opts"
      )
    }
  }

  @Test
  def conflictingSubmodules() = {
    check(
      "conflicting_modules/pom.xml",
      submodules = List(
        "conflicting_modules/module1/pom.xml",
        "conflicting_modules/module1-test/pom.xml"
      )
    ) {
      case (configFile, projectName, List(module1, module2)) =>
        // module1 is "module1"
        // module2 is "module1-test"

        // module1's test configuration should be renamed to avoid conflict with module2
        // Default would be "module1-test", but "module1-test" exists as a reactor artifact (module2)
        // So it should be "module1-test-scope"
        assertEquals("module1", module1.project.name)

        // We need to check the test configuration name for module1.
        // check() function loads the "compile" configuration (default expectation in this test suite assumption?)
        // Actually check() loads the config based on the project file name.
        // module1 comes from "conflicting_modules/module1/pom.xml" -> parent dir is "module1".
        // The bloop file loaded is "module1.json".

        // Let's check the test config of module1
        val module1TestConfigPath =
          configFile.project.directory.resolve(".bloop").resolve("module1-test-scope.json")
        assertTrue(
          s"Test config for module1 should be renamed to module1-test-scope.json",
          Files.exists(module1TestConfigPath)
        )

        val module1TestConfig = readValidBloopConfig(module1TestConfigPath.toFile())
        assertEquals("module1-test-scope", module1TestConfig.project.name)

        // module2 should start with module1-test
        assertEquals("module1-test", module2.project.name)

      case _ =>
        fail("Conflicting modules test should have 2 submodules")
    }
  }

  @Test
  def dependencyTestJars() = {
    check("test_jars/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isDefined)
      assertEquals("2.13.6", configFile.project.`scala`.get.version)
      assertEquals("org.scala-lang", configFile.project.`scala`.get.organization)
      assert(
        !configFile.project.`scala`.get.jars.exists(_.toString.contains("scala3-compiler_3")),
        "No Scala 3 jar should be present."
      )
      assert(!hasCompileClasspathEntryName(configFile, "scala3-library_3"))
      assert(hasCompileClasspathEntryName(configFile, "scala-library"))

      assert(hasTag(configFile, Tag.Library))
      val testJar = configFile.project.resolution.get.modules.find(_.name == "spark-tags_2.13")
      assert(
        testJar.forall(
          _.artifacts.exists(e =>
            e.classifier == Some("sources") && e.path
              .getFileName()
              .toString()
              .endsWith("-test-sources.jar")
          )
        )
      )
      assert(testJar.exists { m =>
        m.artifacts.exists(_.path.toString().endsWith("spark-tags_2.13-3.3.0-tests.jar"))
      })

      assertNoConfigsHaveAnyJars(List(configFile), List(s"$projectName", s"$projectName-test"))
      assertAllConfigsMatchJarNames(List(configFile), List("scala-library", "spark-tags"))
    }
  }

  @Test
  def multiModuleTestJar() = {
    check(
      "multi_module_test_jar/pom.xml",
      submodules = List("multi_module_test_jar/foo/pom.xml", "multi_module_test_jar/bar/pom.xml")
    ) {
      case (configFile, _, List(module1, module2)) =>
        List(configFile, module1, module2).foreach { module =>
          assert(module.project.`scala`.isDefined)
          assertEquals("2.13.8", module.project.`scala`.get.version)
          assertEquals("org.scala-lang", module.project.`scala`.get.organization)
          assert(hasCompileClasspathEntryName(module, "scala-library"))
        }
        assertAllConfigsMatchJarNames(List(configFile, module1, module2), List("scala-library"))

        assertEquals(List("multi_module_test_jar"), module1.project.dependencies)
        assertEquals(List("multi_module_test_jar", "foo-test", "foo"), module2.project.dependencies)

        // The submodules declare scala-maven-plugin executions but no <configuration> (it comes
        // from inherited pluginManagement). "No per-goal override" must still mean "Maven's
        // effective config", so scala-maven-plugin's synthesized default for
        // maven.compiler.target=1.8 (-target:8) must survive rather than collapse to no options.
        List(module1, module2).foreach { module =>
          val opts = module.project.`scala`.get.options
          assert(opts.contains("-target:8"), s"scalac opts: $opts")
        }

      case _ =>
        assert(false, "Multi module test jar should have two submodules")
    }
  }

  @Test
  def testFallbackNamingForTestScope() = {
    check(
      "multi_dependency/pom.xml",
      submodules = List("multi_dependency/module1/pom.xml", "multi_dependency/module2/pom.xml")
    ) {
      case (configFile, projectName, submodulesList) =>
        val (module1: Config.File, module2: Config.File) = submodulesList match {
          case List(m1, m2) => (m1, m2)
          case _ => fail(s"Expected 2 submodules, but got ${submodulesList.size}")
        }

        // Standard naming should be preserved when no collision exists
        assert(!configFile.project.name.contains("-compile"))
        assert(!module1.project.name.contains("-compile"))
        assert(!module2.project.name.contains("-compile"))

      // Note: To test actual collision, we would need to construct a project structure
      // where a submodule name conflicts with the test suffix of another module.
      // For now, we verify that the default behavior is correct (no suffixes).
    }
  }

  @Test
  def junitSupport() = {
    check("junit_project/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)

      // Read the test configuration file
      val testConfigFile = readValidBloopConfig(
        configFile.project.directory.resolve(".bloop").resolve(s"$projectName-test.json").toFile()
      )

      // Check if junit-interface is present in the resolution modules of the test config
      val resolutionModules = testConfigFile.project.resolution.get.modules
      val hasJunitInterface = resolutionModules.exists(_.name.contains("junit-interface"))

      assertTrue(
        "junit-interface should be present in test config when junit is used",
        hasJunitInterface
      )

      // Also check classpath
      val hasJunitInterfaceInClasspath =
        testConfigFile.project.classpath.exists(_.toString.contains("junit-interface"))
      assertTrue(
        "junit-interface should be present in test classpath when junit is used",
        hasJunitInterfaceInClasspath
      )
    }
  }

  @Test
  def issue29() = {
    // The compile and testCompile goals carry different scalac options via per-execution
    // configuration; each bloop project should only see its own (plus shared plugin-level) args.
    check("issue_29/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      val compileOpts = configFile.project.`scala`.get.options
      val testOpts = readValidBloopConfig(
        configFile.project.directory.resolve(".bloop").resolve(s"$projectName-test.json").toFile()
      ).project.`scala`.get.options

      assert(compileOpts.contains("-Ywarn-unused"), s"compile opts: $compileOpts")
      assert(!compileOpts.contains("-Xfatal-warnings"), s"compile opts: $compileOpts")
      assert(testOpts.contains("-Xfatal-warnings"), s"test opts: $testOpts")
      assert(!testOpts.contains("-Ywarn-unused"), s"test opts: $testOpts")

      // A per-execution <args> overrides (replaces) the plugin-level <args>, mirroring Maven's
      // own configuration-merge semantics, so the shared -deprecation does not leak into either.
      assert(!compileOpts.contains("-deprecation"), s"compile opts: $compileOpts")
      assert(!testOpts.contains("-deprecation"), s"test opts: $testOpts")

      // The testCompile-only addScalacArgs must stay out of the compile options: the split
      // config DOMs must not contaminate each other when the union view is built.
      assert(testOpts.contains("-Xtest-only"), s"test opts: $testOpts")
      assert(!compileOpts.contains("-Xtest-only"), s"compile opts: $compileOpts")

      // An execution bound to an unrelated goal (add-source) must not contribute scalac options
      // to either project; only compile/testCompile-bound executions feed the goal-scoped splits.
      assert(!compileOpts.contains("-Xunrelated-goal"), s"compile opts: $compileOpts")
      assert(!testOpts.contains("-Xunrelated-goal"), s"test opts: $testOpts")
    }
  }

  private def check(
      testProject: String,
      submodules: List[String] = Nil,
      extraContent: Map[String, String] = Map.empty,
      prePackage: Boolean = false
  )(
      checking: (Config.File, String, List[Config.File]) => Unit
  ): Unit =
    checkWithOutput(testProject, submodules, extraContent, prePackage) {
      (configFile, projectName, subProjects, _) => checking(configFile, projectName, subProjects)
    }

  // Same as `check`, but also hands the raw Maven output (stdout+stderr) to the assertion so
  // tests can verify behavior that lives in the log, e.g. that the plugin did NOT attempt a
  // remote download for a system-scoped dependency (issue #27).
  private def checkWithOutput(
      testProject: String,
      submodules: List[String] = Nil,
      extraContent: Map[String, String] = Map.empty,
      prePackage: Boolean = false
  )(
      checking: (Config.File, String, List[Config.File], String) => Unit
  ): Unit = {
    println(s"Checking $testProject")
    val tempDir = Files.createTempDirectory("mavenBloop")
    val outFile = copyFromResource(tempDir, testProject)
    extraContent.foreach {
      case (relativePath, content) =>
        val p = tempDir.resolve(relativePath)
        Files.createDirectories(p.getParent)
        Files.write(p, content.getBytes("UTF-8"))
    }
    submodules.foreach(copyFromResource(tempDir, _))
    val wrapperJar = copyFromResource(tempDir, s"maven-wrapper.jar")
    copyFromResource(tempDir, s"maven-wrapper.properties")

    val javaHome = Paths.get(System.getProperty("java.home"))
    val javaArgs = List[String](
      javaHome.resolve("bin/java").toString(),
      "-Dfile.encoding=UTF-8",
      s"-Dmaven.multiModuleProjectDirectory=$tempDir",
      s"-Dmaven.home=$tempDir"
    )

    val jarArgs = List(
      "-jar",
      wrapperJar.toString()
    )

    val bloopProperties: InputStream = getClass().getResourceAsStream("/bloop.properties")

    val properties = new Properties()
    properties.load(bloopProperties)
    val version = properties.get("version").asInstanceOf[String]

    // maven-shade-plugin binds to the `package` phase, while bloopInstall runs at generate-sources.
    // For fixtures that rely on a shaded JAR existing on disk, package the reactor first.
    val packageResult =
      if (prePackage)
        exec(
          List(javaArgs, jarArgs, List("package", "-DskipTests")).flatten,
          outFile.getParent().toFile()
        )
      else Try("")

    val command =
      List(s"ch.epfl.scala:bloop-maven-plugin:$version:bloopInstall", "-DdownloadSources=true")
    val allArgs = List(
      javaArgs,
      jarArgs,
      command
    ).flatten

    val result = exec(allArgs, outFile.getParent().toFile())
    val mavenOutput = result.getOrElse("")
    try {
      val projectPath = outFile.getParent()
      val projectName = projectPath.toFile().getName()
      val bloopDir = projectPath.resolve(".bloop")
      val projectFile = bloopDir.resolve(s"${projectName}.json")

      val configFile = readValidBloopConfig(projectFile.toFile())

      val subProjects = submodules.map { mod =>
        val subProjectName = tempDir.resolve(mod).getParent().toFile().getName()
        val subProjectFile = bloopDir.resolve(s"${subProjectName}.json")
        readValidBloopConfig(subProjectFile.toFile())
      }
      checking(configFile, projectName, subProjects, mavenOutput)
      tempDir.toFile().delete()
      ()
    } catch {
      case NonFatal(e) =>
        if (prePackage) println("Maven package output:\n" + packageResult)
        println("Maven output:\n" + mavenOutput)
        throw e
    }
  }

  private def copyFromResource(
      tempDir: Path,
      filePath: String
  ): Path = {
    val embeddedFile =
      this.getClass.getResourceAsStream(s"/$filePath")
    val outFile = tempDir.resolve(filePath)
    Files.createDirectories(outFile.getParent)
    Files.copy(embeddedFile, outFile, StandardCopyOption.REPLACE_EXISTING)
    outFile
  }

  private def exec(cmd: Seq[String], cwd: File): Try[String] = {
    Try {
      val processBuilder = new ProcessBuilder()
      val out = new StringBuilder()
      processBuilder.directory(cwd)
      processBuilder.command(cmd: _*)
      // Merge stderr into stdout so assertions can see all Maven/plugin log output
      // (e.g. "[ERROR] FAILURE ...:system" lines emitted on a failed resolution).
      processBuilder.redirectErrorStream(true)
      var process = processBuilder.start()

      val reader =
        new BufferedReader(new InputStreamReader(process.getInputStream()))

      var line = reader.readLine()
      while (line != null) {
        out.append(line + "\n")
        line = reader.readLine()
      }

      val exitCode = process.waitFor()

      out.toString()
    }
  }

  @Test
  def withIncludes() = {
    check(
      "with_includes/pom.xml",
      extraContent = Map(
        "with_includes/src/main/resources/included.txt" -> "This file should be included.",
        "with_includes/src/main/resources/excluded.txt" -> "This file should be excluded."
      )
    ) { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      val resources = configFile.project.resources.getOrElse(Nil)

      val included = resources.find(_.toString.endsWith("included.txt"))
      val excluded = resources.find(_.toString.endsWith("excluded.txt"))

      assert(included.isDefined, "included.txt should be in resources")
      assert(excluded.isEmpty, "excluded.txt should NOT be in resources")

      // Ensure the directory itself is not added when we have explicit includes
      val resourceDir = configFile.project.directory.resolve("src/main/resources").toAbsolutePath
      val hasResourceDir = resources.exists(_.toAbsolutePath == resourceDir)
      assert(
        !hasResourceDir,
        s"Resource directory $resourceDir should NOT be in resources when includes are specified"
      )
    }
  }

  @Test
  def issue85() = {
    check(
      "issue_85/pom.xml",
      extraContent = Map(
        "issue_85/LICENSE" -> "LICENSE CONTENT",
        "issue_85/NOTICE" -> "NOTICE CONTENT"
      )
    ) { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      val resources = configFile.project.resources.getOrElse(Nil)
      val license = resources.find(_.toString.endsWith("LICENSE"))
      val notice = resources.find(_.toString.endsWith("NOTICE"))
      assert(license.isDefined, "LICENSE file should be included in resources")
      assert(notice.isDefined, "NOTICE file should be included in resources")

      val baseDirectory = configFile.project.directory.toAbsolutePath
      val hasBaseDir = resources.exists(_.toAbsolutePath == baseDirectory)
      assert(
        !hasBaseDir,
        s"Base directory $baseDirectory should NOT be in resources when includes are specified"
      )
    }
  }

  @Test
  def defaultResources() = {
    check(
      "default_resources/pom.xml",
      extraContent = Map(
        "default_resources/src/main/resources/hello.txt" -> "hello"
      )
    ) { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      val resources = configFile.project.resources.getOrElse(Nil)
      // When no includes/excludes are specified, the whole directory should be included
      val resourceDir = configFile.project.directory.resolve("src/main/resources").toAbsolutePath
      val hasResourceDir = resources.exists(_.toAbsolutePath == resourceDir)
      assert(hasResourceDir, s"Resource directory $resourceDir SHOULD be in resources")

      // Individual files should NOT be listed
      val hasFile = resources.exists(_.toString.endsWith("hello.txt"))
      assert(!hasFile, "Individual files inside resource dir should NOT be in resources list")
    }
  }

  @Test
  def issue84() = {
    check("issue_84/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isDefined, "Scala config should be defined")
      assertEquals("3.4.2", configFile.project.`scala`.get.version)
    }
  }

  @Test
  def exclusion() = {
    check(
      "exclusion/pom.xml",
      submodules = List(
        "exclusion/shims/pom.xml",
        "exclusion/api/pom.xml",
        "exclusion/consumer/pom.xml"
      )
    ) {
      case (_, _, List(_, apiConfig, consumerConfig)) =>
        // api depends on shims — shims output dir must be on api's classpath
        assert(
          hasCompileClasspathEntryName(apiConfig, "shims"),
          "api should have shims on its compile classpath"
        )

        // consumer explicitly excludes shims — its output dir must NOT appear
        assert(
          !hasCompileClasspathEntryName(consumerConfig, "shims"),
          "consumer should not have shims on its compile classpath"
        )

        // api itself must still be on consumer's classpath
        assert(
          hasCompileClasspathEntryName(consumerConfig, "api"),
          "consumer should still have api on its compile classpath"
        )
      case _ =>
        assert(false, "exclusion should have exactly three submodules")
    }
  }

  @Test
  def runtimeDependency() = {
    check("runtime_dependency/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isDefined)

      // logback-classic is runtime-scoped: it must NOT appear on the compile classpath
      assert(
        !hasCompileClasspathEntryName(configFile, "logback-classic"),
        "logback-classic should NOT be on the compile classpath"
      )

      // but it MUST appear in the JVM platform runtime classpath
      assert(
        hasRuntimeClasspathEntryName(configFile, "logback-classic"),
        "logback-classic should be on the JVM platform runtime classpath"
      )

      // transitive runtime dep (logback-core) should also be on runtime classpath
      assert(
        hasRuntimeClasspathEntryName(configFile, "logback-core"),
        "logback-core (transitive of logback-classic) should be on the JVM platform runtime classpath"
      )

      // compile deps must still be present on the runtime classpath
      assert(
        hasRuntimeClasspathEntryName(configFile, "scala-library"),
        "scala-library should still be on the JVM platform runtime classpath"
      )
    }
  }

  @Test
  def issue27() = {
    // A system-scoped dependency must use its provided <systemPath> instead of being
    // resolved from remote repositories (issue #27). Without the fix the plugin attempts a
    // (doomed) remote download and logs "FAILURE ...:system / Could not resolve". We point
    // systemPath at jrt-fs.jar, which exists in any JDK 9+, so this is reliable cross-JDK.
    checkWithOutput("issue_27/pom.xml") { (configFile, projectName, subprojects, mavenOutput) =>
      assert(subprojects.isEmpty)
      assert(configFile.project.`scala`.isDefined)

      // Core regression contract: the plugin must NOT attempt to remotely resolve/download
      // the system-scoped artifact. Before the fix the log contains lines such as
      // "[ERROR] FAILURE ...fake-system-lib...:system" and "Downloading ...fake-system-lib...".
      assertNoRemoteResolution(mavenOutput, "fake-system-lib")

      // The system-scoped jar must appear on the compile classpath via its systemPath.
      assert(
        hasCompileClasspathEntryName(configFile, "jrt-fs.jar"),
        "system-scoped jar (jrt-fs.jar) should be on the compile classpath"
      )

      // It must surface as a resolution module pointing at the provided systemPath jar
      // (jrt-fs.jar) — proving the plugin used the local path, not a remote download.
      val modules = configFile.project.resolution.toList.flatMap(_.modules)
      val systemModule = modules.find(_.name == "fake-system-lib")
      assert(
        systemModule.isDefined,
        "system-scoped dependency should be present as a resolution module"
      )
      assert(
        systemModule.get.artifacts.exists(_.path.toString.endsWith("jrt-fs.jar")),
        "system-scoped module must point at the provided systemPath jar, not a remote download"
      )
    }
  }

  // Asserts the Maven/plugin log shows no attempt to remotely resolve or download the given
  // (system-scoped) artifact: no "FAILURE/Downloading/Could not ... <artifact>" lines.
  private def assertNoRemoteResolution(mavenOutput: String, artifact: String): Unit = {
    val offending = mavenOutput.linesIterator
      .filter(_.contains(artifact))
      .filter(l => l.contains("FAILURE") || l.contains("Downloading") || l.contains("Could not"))
      .toList
    assert(
      offending.isEmpty,
      s"plugin must not remote-resolve system-scoped '$artifact'; offending log lines:\n" +
        offending.mkString("\n")
    )
  }

  @Test
  def issue27MultiModule() = {
    // A multi-module reactor where 'lib' declares a system-scoped dependency and 'app'
    // depends on 'lib'. The whole reactor build must succeed (check() fails if any config
    // is not produced). Note: Maven does NOT propagate a system-scoped dependency
    // transitively across modules, so the jar surfaces only on the declaring module ('lib').
    // A true external dependency-of-dependency behaves the same — the consumer never sees
    // the system artifact in its resolved artifacts — so the meaningful unit of behavior is
    // the declaring module.
    checkWithOutput(
      "issue_27_multimodule/pom.xml",
      submodules = List("issue_27_multimodule/lib/pom.xml", "issue_27_multimodule/app/pom.xml")
    ) {
      case (_, _, List(libConfig, appConfig), mavenOutput) =>
        // The plugin must not attempt to remotely resolve/download the system-scoped artifact.
        assertNoRemoteResolution(mavenOutput, "fake-system-lib")

        // lib declares the system dep: it must be on lib's classpath and resolution modules,
        // pointing at the provided systemPath jar rather than a remote download.
        assert(
          hasCompileClasspathEntryName(libConfig, "jrt-fs.jar"),
          "lib should have the system-scoped jar (jrt-fs.jar) on its compile classpath"
        )
        val libSystemModule =
          libConfig.project.resolution.toList.flatMap(_.modules).find(_.name == "fake-system-lib")
        assert(
          libSystemModule.exists(_.artifacts.exists(_.path.toString.endsWith("jrt-fs.jar"))),
          "lib's system-scoped module must point at the provided systemPath jar"
        )

        // app only depends on lib: per Maven's non-transitive system scope, the system jar
        // does not reach app. Asserting this documents the real (correct) behavior.
        assert(
          !hasCompileClasspathEntryName(appConfig, "jrt-fs.jar"),
          "system-scoped dep must not propagate transitively onto the consumer's classpath"
        )
      case _ =>
        fail("issue_27_multimodule should have exactly two submodules")
    }
  }
}
