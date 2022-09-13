addSbtPlugin("org.scoverage"      % "sbt-scoverage"            % "2.0.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-ghpages"              % "0.6.3")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.6.3")
addSbtPlugin("com.47deg"          % "sbt-microsites"           % "1.3.4")
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                 % "2.3.3")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.10.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.3")
addSbtPlugin("com.github.sbt"     % "sbt-unidoc"               % "0.5.0")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.5.10")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.4.6")

libraryDependencies += "org.scalameta" % "scalameta_2.12" % "4.5.13"
