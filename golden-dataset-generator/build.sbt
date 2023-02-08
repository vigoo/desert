val desertVersion = "0.2.3"

name := "golden-dataset-generator"

lazy val commonSettings = Seq(
  organization  := "io.github.vigoo",
  scalaVersion  := "2.13.10",
  scalacOptions := Seq(
    "-deprecation",
    "-unchecked"
  )
)

lazy val root = Project("golden-dataset-generator", file("."))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"         %% "zio"         % "2.0.0-RC6",
      "dev.zio"         %% "zio-nio"     % "2.0.0-RC7",
      "io.github.vigoo" %% "desert-zio"  % desertVersion,
      "io.github.vigoo" %% "clipp-zio-2" % "0.6.7"
    )
  )
