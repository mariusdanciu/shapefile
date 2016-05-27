
lazy val commonSettings = Seq(
  organization := "net.map",
  version := "0.1",
  scalaVersion := "2.11.8"
)


lazy val shapefileparser = (project in file("shapefileparser")).
  settings(commonSettings: _*).
  settings(
    name := "shapefileparser"
  )


lazy val shapefileui = (project in file("shapefileui")).
  settings(commonSettings: _*).
  settings(
    name := "shapefileui",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-swing" % "2.11+"
    )
  ) dependsOn shapefileparser

lazy val root = (project in file(".")).aggregate(shapefileparser, shapefileui)