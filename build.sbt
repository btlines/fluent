name := "fluent"

version := "0.0.5"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats"      % "0.9.0",
  "com.chuusai"   %% "shapeless" % "2.3.2",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

organization := "beyondthelines"

licenses := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil

bintrayOrganization := Some("beyondthelines")

bintrayPackageLabels := Seq("scala")