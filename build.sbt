import build._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / organization     := "dev.mtomko"

lazy val `gc-rich` = project.in(file("."))
  .settings(
    name := "gc-rich",
    libraryDependencies ++=
      Seq(
        libraries.catsCore,
        libraries.catsEffect,
        libraries.console4cats,
        libraries.decline,
        libraries.declineEffect,
        libraries.declineRefined,
        libraries.fs2Core,
        libraries.fs2Io,
        libraries.refined,
        libraries.scalaTest % Test
      ),
      addCompilerPlugin(libraries.betterMonadicFor)
  )

