name := "genustree"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.apache.commons" % "commons-lang3" % "3.3.2",
  "commons-codec" % "commons-codec" % "1.4",
  "com.typesafe.play" %% "play-ws" % "2.3.7"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)
scalaVersion := "2.11.4"