addSbtPlugin("org.scoverage"      % "sbt-scoverage"            % "2.1.0")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.6.4")
addSbtPlugin("com.47deg"          % "sbt-microsites"           % "1.4.3")
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                 % "2.5.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.16.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.7")
addSbtPlugin("com.github.sbt"     % "sbt-unidoc"               % "0.5.0")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.5.12")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.2")

libraryDependencies += "org.scalameta" % "scalameta_2.12" % "4.9.5"

ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
