import scala.scalanative.build._
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport._
import sbtassembly.AssemblyPlugin.autoImport._
import sbtassembly.{MergeStrategy, PathList}
import org.jetbrains.sbtidea.SbtIdeaPlugin
import org.jetbrains.sbtidea.Keys._
import org.jetbrains.sbtidea.packaging.PackagingMethod

version := "0.1.0-SNAPSHOT"
scalaVersion := "3.7.4"
organization := "chester"

addCommandAlias("updates", "reload plugins; dependencyUpdates; reload return; dependencyUpdates;")

Global / excludeLintKeys ++= Set(
  lspJVM / nativeImageJvm,
  lspJVM / nativeImageVersion,
  intellijBuild,
  intellijPlatform,
  intellijPluginName
)

// Shared settings for all projects
lazy val commonSettings = Seq(
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  scalaVersion := "3.7.4",
  scalacOptions ++= Seq(
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-deprecation",
    // "-Wunused:all", // for scalafix
    // experiemntal:
    "-Yexplicit-nulls"
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
    "-encoding",
    "UTF-8",
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

// https://github.com/effekt-lang/kiama/commit/51fda9aa8386429444f17ab0d9e4b7cab5a06409
lazy val replDependencies = Seq(
  "jline" % "jline" % "2.14.6",
  "org.rogach" %% "scallop" % "6.0.0"
)
lazy val lspDependencies = Seq(
  "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.24.0",
  "com.google.code.gson" % "gson" % "2.13.2"
)
lazy val testingDependencies = Seq(
  "org.scala-sbt" %% "io" % "1.10.5" % Test,
  "org.scalameta" %% "munit" % "1.2.1" % Test
)
lazy val vendoredKiama = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("vendor/kiama"))
  .disablePlugins(ScalafixPlugin)
  .settings(commonVendorSettings)
  .nativeSettings(commonNativeSettings)
  .jvmSettings(
    libraryDependencies ++= (replDependencies ++ lspDependencies ++ testingDependencies)
  )

lazy val KiamaJVM = vendoredKiama.jvm
lazy val KiamaJS = vendoredKiama.js
lazy val KiamaNative = vendoredKiama.native

// JS typings from ScalablyTyped
lazy val jsTypings = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Pure)
  .disablePlugins(ScalafixPlugin)
  .in(file("js-typings"))
  .jsEnablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    commonVendorSettings,
    scalaVersion := "3.5.2" // ScalablyTyped needs older Scala version with implicit.
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
    KiamaJVM,
    KiamaJS,
    KiamaNative,
    jsTypingsJS,
    cliJVM,
    cliJS,
    cliNative,
    lspJVM,
    intellijPlugin
  )
  .settings(
    name := "chester",
    publish / skip := true
  )

// Core library - shared code
lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("modules/core"))
  .dependsOn(utils)
  .settings(commonSettings)
  .settings(
    name := "chester-core",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.2.1" % Test
    )
  )
  .jvmSettings(
    // JVM-specific settings
    libraryDependencies ++= Seq(
      "org.scala-stm" %% "scala-stm" % "0.11.1"
    )
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
  .disablePlugins(ScalafixPlugin)
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
  .dependsOn(vendoredSpire, vendoredKiama)
  .jsConfigure(_.dependsOn(jsTypingsJS))
  .settings(commonSettings)
  .settings(
    name := "chester-utils",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.2.1" % Test,
      "com.lihaoyi" %%% "upickle" % "4.4.1",
      "com.lihaoyi" %%% "fastparse" % "3.1.1",
      "com.lihaoyi" %%% "fansi" % "0.5.1",
      "com.eed3si9n.ifdef" %%% "ifdef-annotation" % "0.4.1",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "org.typelevel" %%% "cats-free" % "2.13.0"
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.11.6",
      "org.jline" % "jline" % "3.30.6",
      "org.jline" % "jline-terminal" % "3.30.6",
      "org.jline" % "jline-terminal-jni" % "3.30.6",
      "org.jline" % "jline-reader" % "3.30.6",
      "org.graalvm.sdk" % "nativeimage" % "24.1.2"
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
      "com.lihaoyi" %%% "os-lib" % "0.11.6"
    )
  )

lazy val utilsJVM = utils.jvm
lazy val utilsJS = utils.js
lazy val utilsNative = utils.native

lazy val cli = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("modules/cli"))
  .dependsOn(core, utils)
  .settings(
    commonSettings,
    name := "chester-cli",
    Compile / mainClass := Some("chester.cli.Main"),
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.2.1" % Test
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings(
    assembly / mainClass := Some("chester.cli.Main"),
    assembly / assemblyJarName := "chester-cli-assembly.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("module-info.class")                           => MergeStrategy.discard
      case PathList("META-INF", "MANIFEST.MF")                  => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.exists(_.toLowerCase.endsWith(".sf"))  => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.exists(_.toLowerCase.endsWith(".dsa")) => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.exists(_.toLowerCase.endsWith(".rsa")) => MergeStrategy.discard
      case PathList("META-INF", "versions", "9", "module-info.class") => MergeStrategy.first
      case PathList("META-INF", xs @ _*)                               => MergeStrategy.first
      case x                                                           => (assembly / assemblyMergeStrategy).value(x)
    }
  )
  .nativeSettings(
    commonNativeSettings,
    Compile / mainClass := Some("chester.cli.Main"),
    nativeConfig ~= (_.withMode(Mode.releaseFull).withLTO(LTO.full))
  )

lazy val cliJVM = cli.jvm
lazy val cliJS = cli.js
lazy val cliNative = cli.native

import sbtnativeimage.NativeImagePlugin
import sbtnativeimage.NativeImagePlugin.autoImport._

lazy val lsp = crossProject(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/lsp"))
  .dependsOn(core, utils)
  .settings(
    commonSettings,
    name := "chester-lsp",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.2.1" % Test
    )
  )
  .jvmSettings(
    Compile / mainClass := Some("chester.lsp.Main"),
    libraryDependencies ++= lspDependencies,
    nativeImageJvm := "graalvm-community",
    nativeImageVersion := "23.0.1"
  )
  .jvmConfigure(_.enablePlugins(NativeImagePlugin))

lazy val lspJVM = lsp.jvm

lazy val intellijPlugin = project
  .in(file("modules/intellij-plugin"))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    commonSettings,
    name := "chester-intellij-plugin",
    intellijPluginName := "chester-intellij-plugin",
    intellijBuild := "242.20224.54",
    intellijPlatform := IntelliJPlatform.IdeaCommunity,
    resolvers += "jitpack" at "https://jitpack.io",
    packageMethod := PackagingMethod.Standalone(),
    libraryDependencies ++= Seq(
      "com.github.ballerina-platform" % "lsp4intellij" % "25ef74cd90"
    )
  )
