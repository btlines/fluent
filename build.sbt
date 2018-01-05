name := "fluent"

version := "0.0.6"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.12", "2.12.4")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "com.chuusai"   %% "shapeless" % "2.3.3",
  "org.scalatest" %% "scalatest" % "3.0.4" % Test
)

organization := "beyondthelines"

licenses := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil

bintrayOrganization := Some("beyondthelines")

bintrayPackageLabels := Seq("scala")