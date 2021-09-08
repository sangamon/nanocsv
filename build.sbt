ThisBuild / scalaVersion     := "3.0.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "de.sangamon"

ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xfatal-warnings", "-source:future")

lazy val root = (project in file("."))
  .settings(
    name := "nanocsv",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
  )
