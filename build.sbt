name := "minesweeper"

organization := "com.skardal"

version := "0.0.1"

scalaVersion := "2.11.2"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test" withSources() withJavadoc()
)

initialCommands := "import com.skardal.minesweeper._"

