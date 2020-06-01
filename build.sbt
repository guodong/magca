name := "magca"

version := "0.1"

val circeVersion = "0.12.3"
lazy val commonSettings = Seq(
  scalaVersion := "2.13.2",

  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)
)

lazy val core = (project in file("core")).settings(
  commonSettings,
  scalacOptions ++= Seq("-Ymacro-annotations"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-graph" %% "graph-core" % "1.13.2"
  )
)

lazy val apps = (project in file("apps")).settings(
  scalacOptions ++= Seq("-Ymacro-annotations", "-Ymacro-debug-lite"),
  commonSettings,
).aggregate(core).dependsOn(core)

