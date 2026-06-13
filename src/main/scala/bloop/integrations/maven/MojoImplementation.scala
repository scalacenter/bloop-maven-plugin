package bloop.integrations.maven

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.JavaConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import bloop.config.Config
import bloop.config.Tag
import org.apache.maven.artifact.Artifact
import org.apache.maven.artifact.ArtifactUtils
import org.apache.maven.execution.MavenSession
import org.apache.maven.model.Resource
import org.apache.maven.plugin.MavenPluginManager
import org.apache.maven.plugin.Mojo
import org.apache.maven.plugin.MojoExecution
import org.apache.maven.plugin.logging.Log
import org.apache.maven.project.MavenProject
import org.codehaus.plexus.util.xml.Xpp3Dom
import org.codehaus.plexus.util.DirectoryScanner
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.artifact.DefaultArtifactType
import org.eclipse.aether.resolution.ArtifactRequest
import scala_maven.AppLauncher

object MojoImplementation {
  private val ScalaMavenGroupArtifact = "net.alchim31.maven:scala-maven-plugin"
  private val JavaMavenGroupArtifact = "org.apache.maven.plugins:maven-compiler-plugin"
  private val ShadeGroupArtifact = "org.apache.maven.plugins:maven-shade-plugin"

  def initializeMojo(
      project: MavenProject,
      session: MavenSession,
      mojoExecution: MojoExecution,
      mavenPluginManager: MavenPluginManager,
      encoding: String
  ): Either[String, BloopMojo] = {
    val buildPlugins = project.getBuild().getPluginsAsMap();

    val (newConfig, moduleType) = Option(buildPlugins.get(ScalaMavenGroupArtifact)) match {
      case None => (None, BloopMojo.ModuleType.JAVA)
      case Some(scalaMavenPlugin) =>
        val pluginConfig = Option(scalaMavenPlugin.getConfiguration).map(_.asInstanceOf[Xpp3Dom])
        val executionConfigs = scalaMavenPlugin.getExecutions.asScala
          .flatMap(e => Option(e.getConfiguration).map(_.asInstanceOf[Xpp3Dom]))
        val combinedConfig = (executionConfigs ++ pluginConfig)
          .reduceOption((dominant, recessive) => Xpp3Dom.mergeXpp3Dom(dominant, recessive))
        (combinedConfig, BloopMojo.ModuleType.SCALA)
    }

    val javaCompilerArgs: List[String] = Option(buildPlugins.get(JavaMavenGroupArtifact)) match {
      case None => List()
      case Some(javaMavenPlugin) =>
        val javaConfig = javaMavenPlugin.getConfiguration.asInstanceOf[Xpp3Dom]
        if (javaConfig != null) {
          val compilerArgs = Option(javaConfig.getChild("compilerArgs"))
          compilerArgs.map(_.getChildren.map(_.getValue).toList).getOrElse(Nil)
        } else List()
    }

    val currentConfig = mojoExecution.getConfiguration
    val dom = newConfig.map(nc => Xpp3Dom.mergeXpp3Dom(nc, currentConfig))
    Try {
      dom.foreach(mojoExecution.setConfiguration)
      val mojo = mavenPluginManager
        .getConfiguredMojo(classOf[Mojo], session, mojoExecution)
        .asInstanceOf[BloopMojo]
      mojo.setModuleType(moduleType)
      mojo.setJavaCompilerArgs(javaCompilerArgs.asJava)
      mojo
    } match {
      case Success(value) => Right(value)
      case Failure(e) => Left(s"Failed to init BloopMojo with conf:\n$dom\n${e.getMessage}")
    }
  }

  def writeCompileAndTestConfiguration(mojo: BloopMojo, session: MavenSession, log: Log): Unit = {
    import scala.collection.JavaConverters._
    def abs(file: File): Path = {
      file.mkdirs()
      file.toPath().toRealPath().toAbsolutePath()
    }

    def resolveArtifact(artifact: Artifact, sources: Boolean = false): Option[File] = {
      val classifierOpt = if (sources) {
        artifact.getType() match {
          case "jar" => Some("sources")
          case "test-jar" => Some("test-sources")
        }
      } else None
      val classifier = classifierOpt.orElse(Option(artifact.getClassifier())).getOrElse("")
      val suffix = if (classifier.nonEmpty) s":$classifier" else ""
      val artifactLog = s"$artifact (sources = $sources)"

      try {
        log.info(s"Resolving artifact: $artifactLog")
        val request = new ArtifactRequest()
        val handler = artifact.getArtifactHandler()
        val artifactType = new DefaultArtifactType(
          artifact.getType(),
          handler.getExtension(),
          handler.getClassifier(),
          handler.getLanguage(),
          handler.isAddedToClasspath(),
          handler.isIncludesDependencies()
        )
        request.setArtifact(
          new DefaultArtifact(
            artifact.getGroupId(),
            artifact.getArtifactId(),
            classifier,
            handler.getExtension(),
            artifact.getVersion(),
            null,
            artifactType
          )
        )
        request.setRepositories(mojo.getRemoteRepositories())
        val result = mojo.getRepoSystem().resolveArtifact(session.getRepositorySession(), request)

        val artifactResult = Option(result.getArtifact().getFile())

        artifactResult match {
          case None => log.warn(s"Resolving $artifactLog returned no files")
          case Some(value) => log.info(s"SUCCESS: $artifactLog")
        }
        artifactResult
      } catch {
        case t: Throwable =>
          log.error(s"FAILURE $artifactLog", t)
          None
      }
    }

    // A reactor module built by maven-shade-plugin rewrites/bundles its bytecode into the shaded
    // JAR; those classes never land in target/classes. Dependents must therefore see the JAR, not
    // the (empty) output directory. Returns the shaded JAR for such a module only when it replaces
    // the module's main artifact AND exists on disk in this reactor build. Returns None when the
    // module is not shaded, when shade attaches the shaded JAR as a secondary (classified) artifact
    // (the main artifact stays unshaded, so target/classes is correct), or when the JAR has not been
    // produced yet (shade binds to `package`, after this goal) — in which case it warns. We never
    // resolve the module's GAV from a repository: that could wire a stale installed/released binary
    // instead of the current build output.
    def shadedJarOf(p: MavenProject): Option[File] = {
      Option(p.getBuild.getPluginsAsMap.get(ShadeGroupArtifact))
        // Only treat it as shade-built when an execution actually binds the `shade` goal; mere
        // plugin/config presence (e.g. shared config, no bound execution) does not produce a JAR.
        .filter(_.getExecutions.asScala.exists(_.getGoals.asScala.contains("shade")))
        .flatMap { plugin =>
        // Merge execution-level config (where the `shade` goal is usually bound) over plugin-level.
        val pluginCfg = Option(plugin.getConfiguration).map(_.asInstanceOf[Xpp3Dom])
        val execCfgs = plugin.getExecutions.asScala
          .flatMap(e => Option(e.getConfiguration).map(_.asInstanceOf[Xpp3Dom]))
        val cfg = (execCfgs ++ pluginCfg)
          .reduceOption((dominant, recessive) => Xpp3Dom.mergeXpp3Dom(dominant, recessive))
        def child(name: String): Option[String] =
          cfg
            .flatMap(d => Option(d.getChild(name)))
            .flatMap(c => Option(c.getValue))
            .map(_.trim)
            .filter(_.nonEmpty)

        val build = p.getBuild
        def warnMissing(jar: File): Option[File] = {
          log.warn(
            s"Reactor module '${p.getArtifactId}' is built by maven-shade-plugin but its shaded " +
              s"JAR was not found at $jar. Its relocated/bundled classes are NOT in target/classes, " +
              s"so dependents will fail to compile. Run `mvn package` on '${p.getArtifactId}' " +
              s"(shade binds to the package phase) before exporting to bloop."
          )
          None
        }

        child("outputFile") match {
          case Some(out) =>
            // Explicit output path; resolve relative entries against the module base directory.
            val f = new File(out)
            val jar = if (f.isAbsolute) f else new File(p.getBasedir, out)
            if (jar.exists()) Some(jar) else warnMissing(jar)
          case None =>
            // With shadedArtifactAttached the shaded JAR is a secondary (classified) artifact and
            // the module's main artifact stays unshaded, so target/classes remains correct.
            val attached = child("shadedArtifactAttached").exists(_.equalsIgnoreCase("true"))
            if (attached) None
            else {
              val base = child("finalName").getOrElse(build.getFinalName)
              val jar = new File(build.getDirectory, s"$base.jar")
              if (jar.exists()) Some(jar) else warnMissing(jar)
            }
        }
      }
    }

    val reactorArtifactIds = session.getProjects().asScala.map(_.getArtifactId).toSet

    def getBloopName(artifactId: String, configuration: String): String = {
      configuration match {
        case "compile" => artifactId
        case _ =>
          val defaultName = s"$artifactId-$configuration"
          if (reactorArtifactIds.contains(defaultName)) s"$artifactId-$configuration-scope"
          else defaultName
      }
    }

    val reactorProjectsSet = mojo
      .getReactorProjects()
      .asScala
      .map { project =>
        (project.getGroupId(), project.getArtifactId(), project.getVersion())
      }
      .toSet

    def isNotReactorProjectArtifact(artifact: Artifact) = {
      !reactorProjectsSet((artifact.getGroupId(), artifact.getArtifactId(), artifact.getVersion()))
    }

    val root = new File(session.getExecutionRootDirectory())
    val project = mojo.getProject()
    val dependencies =
      session.getProjectDependencyGraph.getUpstreamProjects(project, true).asScala.toList

    // Reactor deps whose output we replace with a shaded JAR. These must be dropped from the bloop
    // `dependencies` list as well: a bloop project dependency contributes that project's classesDir
    // (target/classes) to the effective compile classpath, which would re-introduce the empty
    // directory we are substituting away on the classpath. The shaded JAR is a self-contained
    // binary, so the dependent should consume it as a library, not as a project dependency.
    val shadedDeps: Map[MavenProject, File] =
      dependencies.flatMap(d => shadedJarOf(d).map(d -> _)).toMap
    val shadedDepArtifactIds: Set[String] = shadedDeps.keySet.map(_.getArtifactId)

    val dependencyNames = dependencies.flatMap(dep => {
      val matchingArtifacts = project.getArtifacts.asScala.filter(a =>
        ArtifactUtils.versionlessKey(dep.getArtifact) == ArtifactUtils.versionlessKey(a)
      )

      log.info(s"Dependency $dep, $matchingArtifacts")
      val testNames = matchingArtifacts.collect {
        case artifact if artifact.getType == "test-jar" => getBloopName(dep.getArtifactId, "test")
      }.toList
      // Drop only the main project dependency when its output is replaced by a shaded JAR. A
      // consumed test-jar is a separate artifact that shade's main-artifact replacement does not
      // touch, so its dependency name is preserved.
      if (shadedDepArtifactIds.contains(dep.getArtifactId)) testNames
      else testNames.appended(dep.getArtifactId)
    })

    // Map each shaded reactor dep's compile output directory (target/classes) to its shaded JAR, so
    // we can substitute it wherever the stale output directory would otherwise reach a dependent's
    // classpath. Keyed by canonical path to match the explicit list and Maven-resolved entries.
    def canon(p: String): String = new File(p).getCanonicalPath
    val shadedByOutputDir: Map[String, String] =
      shadedDeps.map { case (d, jar) => canon(d.getBuild.getOutputDirectory) -> jar.getAbsolutePath }
    def substituteShaded(path: String): String = shadedByOutputDir.getOrElse(canon(path), path)

    val configDir = mojo.getBloopConfigDir.toPath()
    if (!Files.exists(configDir)) Files.createDirectory(configDir)

    val launcherId = Option(mojo.getLauncher()).filter(_.nonEmpty)
    val launchers = mojo.getLaunchers()
    val launcher = launcherId
      .flatMap(id => launchers.find(_.getId == id))
      .orElse {
        if (launcherId.nonEmpty)
          log.warn(s"Falling back to first launcher: Launcher ID '${launcherId}' does not exist")
        launchers.headOption
      }

    // check if Scala is contained in this project
    // findScalaContext throws an exception if it can't find Scala
    val scalaContext = Try(mojo.findScalaContext()).toOption
    val compileSetup = mojo.getCompileSetup()
    val compilerAndDeps = scalaContext.toList.flatMap(_.findCompilerAndDependencies().asScala)
    val allScalaJars = compilerAndDeps.map { artifact =>
      artifact.getFile().toPath()
    }.toList

    val scalaOrganization = compilerAndDeps
      .collectFirst {
        case artifact
            if artifact.getArtifactId() == "scala3-compiler_3" || artifact
              .getArtifactId() == "scala-compiler" =>
          artifact.getGroupId()
      }
      .getOrElse("org.scala-lang")
    val scalacArgs =
      Try(mojo.getScalacArgs()).toOption.toList.map(_.asScala.toList).flatten.filter(_ != null)

    def writeConfig(
        sourceDirs0: Seq[File],
        classesDir0: File,
        // needs to be lazy, since we resolve artifacts later on
        classpath0: () => java.util.List[_],
        runtimeClasspath0: Option[() => java.util.List[_]],
        resources0: java.util.List[_],
        launcher: Option[AppLauncher],
        configuration: String
    ): Unit = {
      val name = getBloopName(project.getArtifactId(), configuration)
      val build = project.getBuild()
      val baseDirectory = abs(project.getBasedir())
      val out = baseDirectory.resolve("target")
      val analysisOut = None
      val sourceDirs = sourceDirs0.map(abs).toList
      val classesDir = abs(classesDir0)
      val artifacts = project.getArtifacts().asScala
      lazy val libraryAndDependencies = scalaContext.toList
        .flatMap(_.findLibraryAndDependencies().asScala)

      // if we don't add scala-library explicitely it will not be available in artifacts
      val hasScalaLibrary = artifacts.exists {
        case a: Artifact => a.getArtifactId() == "scala-library"
      }
      val allArtifacts = if (hasScalaLibrary) artifacts else artifacts ++ libraryAndDependencies
      val isJar = Set("jar", "test-jar")
      val modules0 =
        allArtifacts.collect {
          case art: Artifact if isJar(art.getType()) && isNotReactorProjectArtifact(art) =>
            if (art.getArtifactId() == "scala-library")
              scalaContext match {
                case Some(context) =>
                  /* If the scala library is not specified explicitely as recommended
                   * it might sometimes be wrong, this doesn't happen for Scala 3.
                   */
                  val scalaVersion = context.version().toString()
                  if (!scalaVersion.startsWith("3.") && scalaVersion != art.getVersion())
                    art.setVersion(context.version.toString)
                case _ =>
              }
            resolveArtifact(art).foreach { resolvedFile =>
              // since we don't resolve dependencies automatically in the plugin, this will be null
              art.setFile(resolvedFile)
            }
            if (mojo.shouldDownloadSources()) {
              resolveArtifact(art, sources = true)
            }
            artifactToConfigModule(art, project, session)
        }

      val (modules, extraClasspath) = {
        val hasJunit =
          allArtifacts.exists(a => a.getGroupId == "junit" && a.getArtifactId == "junit")
        val hasJunitInterface = allArtifacts.exists(a => a.getArtifactId == "junit-interface")
        if (hasJunit && !hasJunitInterface && configuration == "test") {
          val junitInterfaceVersion = "0.13.3"
          val artifact = new org.apache.maven.artifact.DefaultArtifact(
            "com.github.sbt",
            "junit-interface",
            junitInterfaceVersion,
            "test",
            "jar",
            "",
            new org.apache.maven.artifact.handler.DefaultArtifactHandler("jar")
          )
          resolveArtifact(artifact) match {
            case Some(file) =>
              artifact.setFile(file)
              val module = artifactToConfigModule(artifact, project, session)
              (modules0.toList :+ module, List(file.toPath))
            case None =>
              (modules0.toList, Nil)
          }
        } else {
          (modules0.toList, Nil)
        }
      }

      val resolution = Some(Config.Resolution(modules))

      val (classpath, runtimeClasspath) = {
        // keys of artifacts that survived Maven's exclusion resolution for this module.
        val resolvedArtifactKeys: Set[String] = project.getArtifacts.asScala
          .map(a => ArtifactUtils.versionlessKey(a))
          .toSet

        val projectDependencies = dependencies.flatMap { d =>
          if (!resolvedArtifactKeys.contains(ArtifactUtils.versionlessKey(d.getArtifact))) Nil
          else {
            val build = d.getBuild()
            val dirs =
              if (configuration == "compile") build.getOutputDirectory() :: Nil
              else build.getTestOutputDirectory() :: build.getOutputDirectory() :: Nil
            dirs.map(substituteShaded)
          }
        }

        val cp = classpath0().asScala.toList
          .asInstanceOf[List[String]]
          .map(u => abs(new File(substituteShaded(u))))
        // scalaLibrary might not be added by default to classpath and it's needed for the compilation
        val hasScalaLibrary = cp.exists(p => p.toFile().getName().contains("scala_library-"))

        val fullClasspath =
          if (hasScalaLibrary) cp else cp ++ libraryAndDependencies.map(_.getFile().toPath())
        val compileCp =
          (projectDependencies.map(u => abs(new File(u))) ++ fullClasspath ++ extraClasspath).toList

        val runtimeCp = runtimeClasspath0.flatMap { getRuntimeCp =>
          val runtimeElements =
            getRuntimeCp().asScala.toList.asInstanceOf[List[String]].map(u => abs(new File(u)))
          Some((compileCp ++ runtimeElements).distinct)
        }

        (compileCp, runtimeCp)
      }

      val tags = if (configuration == "test") List(Tag.Test) else List(Tag.Library)

      // add main dependency to test project
      val fullDependencies =
        if (configuration == "test") project.getArtifactId :: dependencyNames else dependencyNames

      // FORMAT: OFF
      val config = {
        val sbt = None
        val test = Some(Config.Test.defaultConfiguration)
        val java = Some(Config.Java(mojo.getJavacArgs().asScala.toList))
        val `scala` =
          scalaContext.map{
            context =>
              Config.Scala(scalaOrganization, mojo.getScalaArtifactID(), context.version().toString(), scalacArgs, allScalaJars, analysisOut, Some(compileSetup), None)
          }
        val javaHome = Some(abs(mojo.getJavaHome().getParentFile.getParentFile))
        val jvmArgs = launcher.map(_.getJvmArgs.toList).getOrElse(List.empty)
        val mainClass = launcher.map(_.getMainClass).filter(_.nonEmpty)
        val platform = Some(Config.Platform.Jvm(Config.JvmConfig(javaHome, jvmArgs), mainClass, None, runtimeClasspath, None))
        val resources = Some(resources0.asScala.toList.flatMap {
          case a: Resource =>
            val dir = Paths.get(a.getDirectory())
            if (Files.exists(dir)) {
              if (a.getIncludes().isEmpty() && a.getExcludes().isEmpty()) {
                List(dir)
              } else {
                val scanner = new DirectoryScanner()
                scanner.setBasedir(a.getDirectory())
                scanner.setIncludes(a.getIncludes().toArray(new Array[String](0)))
                scanner.setExcludes(a.getExcludes().toArray(new Array[String](0)))
                scanner.scan()
                scanner.getIncludedFiles().toList.map(f => dir.resolve(f))
              }
            } else Nil
          case _ => Nil
        })
        val project = Config.Project(name, baseDirectory, Some(root.toPath), sourceDirs, None, None, fullDependencies, classpath, out, classesDir, resources, `scala`, java, sbt, test, platform, resolution, Some(tags), None)
        Config.File(Config.File.LatestVersion, project)
      }
      // FORMAT: ON

      val configTarget = new File(mojo.getBloopConfigDir, s"$name.json")
      val finalTarget = relativize(root, configTarget).getOrElse(configTarget.getAbsolutePath)
      log.info(s"Generated $finalTarget")
      log.debug(s"Configuration to be serialized:\n$config")
      bloop.config.write(config, configTarget.toPath)

      log.info(
        s"Starting to write configuration for project: ${project.getArtifactId()} with configuration: $configuration"
      )
      log.debug(s"Source directories: ${sourceDirs0.map(_.getAbsolutePath).mkString(", ")}")
      log.debug(s"Classpath: ${classpath0().asScala.mkString(", ")}")
      log.debug(s"Resources: ${resources0.asScala.mkString(", ")}")
      log.debug(s"Output directory: ${classesDir0.getAbsolutePath}")
    }

    writeConfig(
      mojo.getCompileSourceDirectories.asScala.toSeq,
      mojo.getCompileOutputDir,
      project.getCompileClasspathElements,
      Some(() => project.getRuntimeClasspathElements()),
      project.getResources,
      launcher,
      "compile"
    )

    writeConfig(
      mojo.getTestSourceDirectories.asScala.toSeq,
      mojo.getTestOutputDir,
      project.getTestClasspathElements,
      None,
      project.getTestResources,
      launcher,
      "test"
    )
  }

  private def relativize(base: File, file: File): Option[String] = {
    import scala.util.control.Exception.catching
    val basePath = (if (base.isAbsolute) base else base.getCanonicalFile).toPath
    val filePath = (if (file.isAbsolute) file else file.getCanonicalFile).toPath
    if ((filePath startsWith basePath) || (filePath.normalize() startsWith basePath.normalize())) {
      val relativePath =
        catching(classOf[IllegalArgumentException]) opt (basePath relativize filePath)
      relativePath map (_.toString)
    } else None
  }

  private def artifactToConfigModule(
      artifact: Artifact,
      project: MavenProject,
      session: MavenSession
  ): Config.Module = {
    val base = session.getLocalRepository().getBasedir()
    val artifactRelativePath = session.getLocalRepository().pathOf(artifact)
    val sources =
      artifact.getType() match {
        case "test-jar" => artifactRelativePath.replace("-tests.jar", "-test-sources.jar")
        case _ => artifactRelativePath.replace(".jar", "-sources.jar")
      }
    val sourcesJarPath = Paths.get(base).resolve(sources)
    val sourcesList = if (sourcesJarPath.toFile().exists()) {
      List(
        Config.Artifact(
          name = artifact.getArtifactId(),
          classifier = Option("sources"),
          checksum = None,
          path = sourcesJarPath
        )
      )
    } else {
      Nil
    }
    if (artifact.getFile() == null)
      throw new IllegalArgumentException(s"Could not resolve $artifact")
    Config.Module(
      organization = artifact.getGroupId(),
      name = artifact.getArtifactId(),
      version = artifact.getVersion(),
      configurations = None,
      Config.Artifact(
        name = artifact.getArtifactId(),
        classifier = Option(artifact.getClassifier()),
        checksum = None,
        path = artifact.getFile().toPath()
      ) :: sourcesList
    )
  }
}
