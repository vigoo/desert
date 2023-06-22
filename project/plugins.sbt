addSbtPlugin("org.scoverage"      % "sbt-scoverage"            % "2.0.8")
addSbtPlugin("com.typesafe.sbt"   % "sbt-ghpages"              % "0.6.3")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.6.4")
addSbtPlugin("com.47deg"          % "sbt-microsites"           % "1.3.4")
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                 % "2.3.7")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.13.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.5")
addSbtPlugin("com.github.sbt"     % "sbt-unidoc"               % "0.5.0")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.5.10")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.0")

libraryDependencies += "org.scalameta" % "scalameta_2.12" % "4.7.8"

libraryDependencies += "org.scoverage" %% "scalac-scoverage-plugin" % "2.0.10" cross (CrossVersion.full)

ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
