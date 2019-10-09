name := "sgit"

version := "0.1"

scalaVersion := "2.13.1"

//Tests
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"
parallelExecution in Test := false

//Parses the command line arguments and options
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"

// file I/O
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0"

