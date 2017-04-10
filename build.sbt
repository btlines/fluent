name := "Fluent"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats"      % "0.8.1",
  "com.chuusai"   %% "shapeless" % "2.3.2",

  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)


    