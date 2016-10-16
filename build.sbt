lazy val commonSettings = Seq(
  organization := "org.me",
  version := "1.0",
  scalaVersion := "2.11.8")

lazy val root = (project in file("."))
    .settings(commonSettings: _*)
    .settings(
      name := "ml_fun",
      libraryDependencies ++= Seq(
        "org.me" %% "scalacommon" % "1.0" changing(),
        "org.scalatest" %% "scalatest" % "2.2.6",
        "org.typelevel" %% "cats-core" % "0.7.2",
        "org.typelevel" %% "cats-kernel" % "0.7.2",
        "org.typelevel" %% "cats-macros" % "0.7.2",
        "org.scalacheck" %% "scalacheck" % "1.12.1"))

