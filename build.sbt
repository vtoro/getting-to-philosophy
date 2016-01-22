name := "getting-to-philosophy"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq (
  "-Xlint",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings"
)

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.spire-math" %%% "cats" % "0.4.0-SNAPSHOT"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

enablePlugins(ScalaJSPlugin)
