val scala3Version = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sic_types",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions += "-Xfatal-warnings",

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
