
lazy val commonSettings = Seq(
  organization := "net.map",
  version := "0.1",
  scalaVersion := "2.11.8"
)



lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "shapefile",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-swing" % "2.11+"
    )
  )