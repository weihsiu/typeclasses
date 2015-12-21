name := "typeclasses"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.11.7"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.5.0"
)
