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

      case _ =>
        assert(false, "Multi module test jar should have two submodules")
    }
  }

  @Test
  def junitSupport() = {
    check("junit_project/pom.xml") { (configFile, projectName, subprojects) =>
      assert(subprojects.isEmpty)
      
      // Read the test configuration file
      val testConfigFile = readValidBloopConfig(configFile.project.directory.resolve(".bloop").resolve(s"$projectName-test.json").toFile())
      
      // Check if junit-interface is present in the resolution modules of the test config
      val resolutionModules = testConfigFile.project.resolution.get.modules
      val hasJunitInterface = resolutionModules.exists(_.name.contains("junit-interface"))
      
      assertTrue("junit-interface should be present in test config when junit is used", hasJunitInterface)
      
      // Also check classpath
      val hasJunitInterfaceInClasspath = testConfigFile.project.classpath.exists(_.toString.contains("junit-interface"))
      assertTrue("junit-interface should be present in test classpath when junit is used", hasJunitInterfaceInClasspath)
    }
  }

  private def check(testProject: String, submodules: List[String] = Nil, extraContent: Map[String, String] = Map.empty)(
      checking: (Config.File, String, List[Config.File]) => Unit
  ): Unit = {
    println(s"Checking $testProject")
    val tempDir = Files.createTempDirectory("mavenBloop")
    val outFile = copyFromResource(tempDir, testProject)
    extraContent.foreach { case (relativePath, content) =>
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

    val command =
      List(s"ch.epfl.scala:bloop-maven-plugin:$version:bloopInstall", "-DdownloadSources=true")
    val allArgs = List(
      javaArgs,
      jarArgs,
      command
    ).flatten

    val result = exec(allArgs, outFile.getParent().toFile())
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
      checking(configFile, projectName, subProjects)
      tempDir.toFile().delete()
      ()
    } catch {
      case NonFatal(e) =>
        println("Maven output:\n" + result)
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
      processBuilder.command(cmd: _*);
      var process = processBuilder.start()

      val reader =
        new BufferedReader(new InputStreamReader(process.getInputStream()))

      var line = reader.readLine()
      while (line != null) {
        out.append(line + "\n")
        line = reader.readLine()
      }
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
      assert(!hasResourceDir, s"Resource directory $resourceDir should NOT be in resources when includes are specified")
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
      assert(!hasBaseDir, s"Base directory $baseDirectory should NOT be in resources when includes are specified")
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

}
