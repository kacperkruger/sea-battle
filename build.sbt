ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "sea_battle"
  )

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.14"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"
libraryDependencies += "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
