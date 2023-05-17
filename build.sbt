import sbt.Keys.libraryDependencies
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val root = project
  .in(file("."))
  .aggregate(mafJVM, mafJS)

lazy val maf = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("code"))
  .settings(
    /** General settings */
    name := "maf",
    organization := "soft",
    version := "2.0",
    scalaVersion := "3.2.1",
    //crossScalaVersions ++= Seq("2.13.6", "3.1.0"),
    /** Dependencies */
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
    /** Compilation options */
    maxErrors := 5,
    /** Configuration for running the tests */
    Test / logBuffered := false,
    Test / testOptions += Tests.Argument("-oI"), // Produces a summary after running the tests, showing the failing tests
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.15" % "test",
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.16.0" % "test",
    libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-17" % "3.2.15.0" % "test",
    /** Imported options from https://tpolecat.github.io/2017/04/25/scalac-flags.html */
    scalacOptions ++= Seq(
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      //"-encoding",
      // "-explain",
      "-new-syntax",
      "-Yexplicit-nulls",
      //"-explaintypes", // Explain type errors in more detail.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros", // Allow macro definition (besides implementation and application)
      "-language:higherKinds", // Allow higher-kinded types
      "-language:implicitConversions", // Allow definition of implicit functions called views
      "-unchecked", // Enable additional warnings where generated code depends on assumptions.
      //"-Ycheck-init", // Wrap field accessors to throw an exception on uninitialized access.
      //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
      //"-Xfuture",                          // Turn on future language features.
      //"-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
      //"-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
      //"-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
      //"-Xlint:delayedinit-select", // Selecting member of DelayedInit.
      //"-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
      //"-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
      //"-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
      //"-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
      //"-Xlint:nullary-unit", // Warn when nullary methods return Unit.
      //"-Xlint:option-implicit", // Option.apply used implicit view.
      //"-Xlint:package-object-classes", // Class or object defined in package object.
      //"-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
      //"-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
      //"-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
      //"-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
      //"-Xlint:unsound-match",              // Pattern match may not be typesafe.
      //"-Ypartial-unification",             // Enable partial unification in type constructor inference
      //"-Ywarn-dead-code", // Warn when dead code is identified.
      //"-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
      //"-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
      //"-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
      //"-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      //"-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
      //"-Ywarn-numeric-widen", // Warn when numerics are widened.
      //"-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
      //"-Ywarn-unused:imports", // Warn if an import selector is not referenced.
      //"-Ywarn-unused:locals", // Warn if a local definition is unused.
      //"-Ywarn-unused:params",              // Warn if a value parameter is unused.
      //"-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
      //"-Ywarn-unused:privates" // Warn if a private member is unused.
      // "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
    )
  )
  .jvmSettings(
    assembly / mainClass := Some("maf.cli.runnables.Benchmark"),
    /** General */
    Compile / mainClass := Some("maf.cli.runnables.Benchmark"),
    //libraryDependencies += "net.openhft" % "affinity" % "3.21ea82",
    run / fork := false,
  )
  .jvmConfigure(_.enablePlugins(JmhPlugin))
  .jsSettings(
    /** Dependencies */
    libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "1.1.0").cross(CrossVersion.for3Use2_13)
  )
  .nativeSettings(
    Compile / mainClass := Some("maf.cli.runnables.AnalyzeProgram"),
    nativeMode := "release-full",
    nativeLTO := "thin"
  )

lazy val mafJVM = maf.jvm
lazy val mafJS = maf.js
lazy val mafNative = maf.native