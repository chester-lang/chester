import scala.scalanative.build._

version := "0.1.0-SNAPSHOT"
scalaVersion := "3.7.4"
organization := "chester"

// Shared settings for all projects
lazy val commonSettings = Seq(
  scalaVersion := "3.7.4",
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-deprecation"
  ),
  // Workaround for Metals: disable BSP for native/js targets to prevent compilation issues
  // See: https://github.com/scalameta/metals-feature-requests/issues/13
  bspEnabled := {
    val platform = crossProjectPlatform.?.value.getOrElse(JVMPlatform)
    platform == JVMPlatform
  }
)

// Shared native settings
lazy val commonNativeSettings = Seq(
  nativeConfig ~= {
    _.withLTO(LTO.none)
      .withMode(Mode.debug)
      .withGC(GC.immix)
  }
)

// Vendor settings for spire-native (Scala 3.4 migration)
lazy val commonVendorSettings = Seq(
  scalaVersion := "3.7.4",
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-language:implicitConversions",
    "-nowarn"
  ),
  // Workaround for Metals: disable BSP for native/js targets to prevent compilation issues
  // See: https://github.com/scalameta/metals-feature-requests/issues/13
  bspEnabled := {
    val platform = crossProjectPlatform.?.value.getOrElse(JVMPlatform)
    platform == JVMPlatform
  }
)

// original kiama-core
lazy val vendoredKiamaCore = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("vendor/kiama-core"))
  .settings(commonVendorSettings)
  .nativeSettings(commonNativeSettings)

lazy val kiamaCoreJVM = vendoredKiamaCore.jvm
lazy val kiamaCoreJS = vendoredKiamaCore.js
lazy val kiamaCoreNative = vendoredKiamaCore.native

// JS typings from ScalablyTyped
lazy val jsTypings = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("js-typings"))
  .jsEnablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    commonVendorSettings,
  scalaVersion := "3.5.2", // ScalablyTyped needs older Scala version with implicit.
  )
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "@types/node" -> "22.7.0" // needs 22.7.0. newer versions have issues
    )
  )

lazy val jsTypingsJS = jsTypings.js

// Root project
lazy val root = project
  .in(file("."))
  .aggregate(
    coreJVM,
    coreJS,
    coreNative,
    utilsJVM,
    utilsJS,
    utilsNative,
    vendoredSpireJVM,
    vendoredSpireJS,
    vendoredSpireNative,
    kiamaCoreJVM,
    kiamaCoreJS,
    kiamaCoreNative,
    jsTypingsJS
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
    commonNativeSettings
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

// commit 0fe5a6a9714181a20fc9cef4c8b2af088ff2b4c9, core & util & platform & macros, main only, no tests
// rewrite by scalac with 3.4-migration
// needed project/GenProductTypes.scala
// because spire doesn't provide scala native 0.5 binary for now
lazy val genProductTypes = TaskKey[Seq[File]](
  "gen-product-types",
  "Generates several type classes for Tuple2-22."
)
lazy val vendoredSpire = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("vendor/spire"))
  .settings(
    name := "spire",
    commonVendorSettings,
    scalacOptions ++= Seq("-rewrite", "-source", "3.4-migration"),
    Compile / sourceGenerators += (Compile / genProductTypes).taskValue,
    genProductTypes := {
      val scalaSource = (Compile / sourceManaged).value
      val s = streams.value
      s.log.info("Generating spire/std/tuples.scala")
      val algebraSource = ProductTypes.algebraProductTypes
      val algebraFile = (scalaSource / "spire" / "std" / "tuples.scala").asFile
      IO.write(algebraFile, algebraSource)

      Seq[File](algebraFile)
    },
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "algebra-laws" % "2.13.0"
    )
  )
  .nativeSettings(
    commonNativeSettings
  )

lazy val vendoredSpireJVM = vendoredSpire.jvm
lazy val vendoredSpireJS = vendoredSpire.js
lazy val vendoredSpireNative = vendoredSpire.native

// Utils library - another subproject
lazy val utils = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("modules/utils"))
  .dependsOn(core, vendoredSpire, vendoredKiamaCore)
  .jsConfigure(_.dependsOn(jsTypingsJS))
  .settings(commonSettings)
  .settings(
    name := "chester-utils",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.0.0" % Test,
      "com.lihaoyi" %%% "upickle" % "4.0.2",
      "com.lihaoyi" %%% "fastparse" % "3.1.1",
      "com.lihaoyi" %%% "fansi" % "0.5.1",
      "com.eed3si9n.ifdef" %%% "ifdef-annotation" % "0.4.1",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "org.typelevel" %%% "cats-free" % "2.13.0"
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.11.3",
      "org.jline" % "jline" % "3.30.6",
      "org.jline" % "jline-terminal" % "3.30.6",
      "org.jline" % "jline-terminal-jni" % "3.30.6",
      "org.jline" % "jline-reader" % "3.30.6"
    )
  )
  .jsConfigure(project => project.enablePlugins(ScalaJSBundlerPlugin))
  .jsSettings(
    scalaJSUseMainModuleInitializer := false,
    Compile / npmDependencies ++= Seq(
      "@types/node" -> "22.7.0"
    )
  )
  .nativeSettings(
    commonNativeSettings,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "os-lib" % "0.11.3"
    )
  )

lazy val utilsJVM = utils.jvm
lazy val utilsJS = utils.js
lazy val utilsNative = utils.native
