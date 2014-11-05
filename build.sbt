name := "famit"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.apache.commons" % "commons-lang3" % "3.3.2"
)     

play.Project.playScalaSettings
