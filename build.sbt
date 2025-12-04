import scala.scalanative.build._

version := "0.1.0-SNAPSHOT"
scalaVersion := "3.7.4"
organization := "chester"

// Shared settings for all projects
lazy val commonSettings = Seq(
  scalaVersion := "3.7.3",
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-deprecation"
  )
)

// Root project
lazy val root = project
  .in(file("."))
  .aggregate(
    coreJVM,
    coreJS,
    coreNative,
    utilsJVM,
    utilsJS,
    utilsNative
  )
  .settings(
    name := "chester",
    publish / skip := true
  )

// Core library - shared code
lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    name := "chester-core",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.0.0" % Test
    )
  )
  .jvmSettings(
    // JVM-specific settings
  )
  .jsSettings(
    // Scala.js-specific settings
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
  )
  .nativeSettings(
    // Scala Native-specific settings
    nativeConfig ~= {
      _.withLTO(LTO.none)
        .withMode(Mode.debug)
        .withGC(GC.immix)
    }
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

// Utils library - another subproject
lazy val utils = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("modules/utils"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "chester-utils",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.0.0" % Test
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := false
  )
  .nativeSettings(
    nativeConfig ~= {
      _.withLTO(LTO.none)
        .withMode(Mode.debug)
        .withGC(GC.immix)
    }
  )

lazy val utilsJVM = utils.jvm
lazy val utilsJS = utils.js
lazy val utilsNative = utils.native
