name := "fun-scala"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4.9" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
