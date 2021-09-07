ThisBuild / scalaVersion     := "3.0.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "de.sangamon"


lazy val root = (project in file("."))
  .settings(
    name := "nanocsv"
  )
