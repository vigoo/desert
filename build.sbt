import microsites.ConfigYml
import sbt.Keys.scalacOptions
import sbtcrossproject.{CrossProject, CrossType}
import scoverage.ScoverageKeys.coverageEnabled
import xerial.sbt.Sonatype._

val scala2 = "2.13.10"
val scala3 = "3.2.2"

val zioVersion       = "2.0.13"
val zioSchemaVersion = "0.4.10"

name := "desert"

ThisBuild / dynverSonatypeSnapshots := true

lazy val commonSettings = Seq(
  organization                        := "io.github.vigoo",
  scalaVersion                        := scala2,
  crossScalaVersions                  := Seq(scala2, scala3),
  libraryDependencies ++=
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq.empty
      case _            =>
        Seq(
          compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
        )
    }),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio"               % zioVersion % Test,
    "dev.zio" %% "zio-test"          % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt"      % zioVersion % Test,
    "dev.zio" %% "zio-test-magnolia" % zioVersion % Test
  ),
  Test / compile / coverageEnabled    := true,
  Compile / compile / coverageEnabled := false,
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) =>
      Seq(
        "-deprecation",
        "-unchecked",
        "-Yrangepos"
      )
    case Some((3, _))  =>
      Seq(
        "-deprecation",
        "-Ykind-projector"
      )
    case _             => Nil
  }),
  // Publishing
  publishMavenStyle                   := true,
  licenses                            := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  sonatypeProjectHosting              := Some(GitHubHosting("vigoo", "desert", "daniel.vigovszky@gmail.com")),
  developers                          := List(
    Developer(
      id = "vigoo",
      name = "Daniel Vigovszky",
      email = "daniel.vigovszky@gmail.com",
      url = url("https://vigoo.github.io")
    )
  ),
  sonatypeCredentialHost              := "s01.oss.sonatype.org",
  sonatypeRepository                  := "https://s01.oss.sonatype.org/service/local",
  credentials ++=
    (for {
      username <- Option(System.getenv().get("SONATYPE_USERNAME"))
      password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
    } yield Credentials("Sonatype Nexus Repository Manager", "s01.oss.sonatype.org", username, password)).toSeq,
  resolvers +=
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

lazy val root = Project("desert", file("."))
  .settings(commonSettings)
  .settings(
    publishArtifact    := false,
    description        := "A Scala binary serialization library",
    crossScalaVersions := Nil
  )
  .aggregate(
    core.jvm,
    core.js,
    shapeless.jvm,
    shapeless.js,
    akka,
    cats.jvm,
    cats.js,
    catsEffect.jvm,
    catsEffect.js,
    zio.jvm,
    zio.js,
    zioSchema.jvm,
    zioSchema.js,
    shardcake,
    benchmarks
  )

lazy val core = CrossProject("desert-core", file("desert-core"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "A Scala binary serialization library"
  )
  .jsSettings(coverageEnabled := false)
  .enablePlugins(TupleCodecGenerator)

lazy val shapeless = CrossProject("desert-shapeless", file("desert-shapeless"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "Shapeless based generic derivation for desert codecs",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai"   %% "shapeless"     % "2.3.10"
    ),
    crossScalaVersions -= scala3
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val akka = Project("desert-akka", file("desert-akka"))
  .settings(commonSettings)
  .settings(
    description := "Akka serialization bindings for desert",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor"       % "2.6.19",
      "com.typesafe.akka" %% "akka-actor-typed" % "2.6.19"
    )
  )
  .dependsOn(core.jvm)

lazy val cats = CrossProject("desert-cats", file("desert-cats"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "Desert serializers for cats data types",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"        % "2.9.0",
      "dev.zio"       %% "zio-interop-cats" % "23.0.0.6" % Test
    )
  )
  .jsSettings(coverageEnabled := false)
  .dependsOn(core % "compile->compile;test->test")

lazy val catsEffect = CrossProject("desert-cats-effect", file("desert-cats-effect"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "Cats-effect API bindings for desert",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.4.9"
    )
  )
  .jsSettings(coverageEnabled := false)
  .dependsOn(core)

lazy val zio = CrossProject("desert-zio", file("desert-zio"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "ZIO API and codecs for desert",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"         % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion
    )
  )
  .jsSettings(coverageEnabled := false)
  .dependsOn(core)

lazy val zioPrelude = CrossProject("desert-zio-prelude", file("desert-zio-prelude"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "ZIO Prelude codecs and monadic API for desert",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-prelude" % "1.0.0-RC16"
    )
  )
  .dependsOn(core % "compile->compile;test->test", zio)

lazy val zioSchema = CrossProject("desert-zio-schema", file("desert-zio-schema"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "ZIO Schema based generic derivation and bindings for desert",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-schema"            % zioSchemaVersion,
      "dev.zio" %% "zio-schema-derivation" % zioSchemaVersion,
      "dev.zio" %% "zio-schema-zio-test"   % zioSchemaVersion % Test
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) =>
          Seq(
            "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
          )
        case _            => Seq()
      }
    }
  )
  .dependsOn(core % "compile->compile;test->test", zio)

lazy val shardcake = Project("desert-shardcake", file("desert-shardcake"))
  .settings(commonSettings)
  .settings(
    description := "Shardcake serialization bindings for desert",
    libraryDependencies ++= Seq(
      "com.devsisters" %% "shardcake-core"        % "2.0.6",
      "dev.zio"        %% "zio-schema-derivation" % zioSchemaVersion % Test
    )
  )
  .dependsOn(core.jvm, zio.jvm)
  .dependsOn(zioSchema.jvm % "test->compile")

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(
    publishArtifact := false,
    coverageEnabled := false,
    crossScalaVersions -= scala3
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core.jvm, shapeless.jvm, zioSchema.jvm)

lazy val docsPlugins = project
  .in(file("docs-plugins"))
  .settings(commonSettings)
  .settings(
    publishArtifact := false,
    coverageEnabled := false,
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "mdoc-cli" % "2.3.7"
    )
  )
  .dependsOn(core.jvm)

lazy val docs = project
  .settings(commonSettings)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(SiteScaladocPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(MicrositesPlugin)
  .settings(
    name                                       := "desert",
    description                                := "A Scala binary serialization library",
    publishArtifact                            := false,
    crossScalaVersions -= scala3,
    ScalaUnidoc / siteSubdirName               := "api",
    addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, ScalaUnidoc / siteSubdirName),
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
      core.js,
      catsEffect.js,
      zio.js,
      cats.js,
      benchmarks,
      shapeless.js,
      zioPrelude.js,
      zioSchema.js
    ),
    git.remoteRepo                             := "git@github.com:vigoo/desert.git",
    micrositeUrl                               := "https://vigoo.github.io",
    micrositeBaseUrl                           := "/desert",
    micrositeHomepage                          := "https://vigoo.github.io/desert/",
    micrositeDocumentationUrl                  := "/desert/docs",
    micrositeAuthor                            := "Daniel Vigovszky",
    micrositeTwitterCreator                    := "@dvigovszky",
    micrositeGithubOwner                       := "vigoo",
    micrositeGithubRepo                        := "desert",
    micrositeGitterChannel                     := false,
    micrositeDataDirectory                     := file("docs/src/microsite/data"),
    micrositeStaticDirectory                   := file("docs/src/microsite/static"),
    micrositeImgDirectory                      := file("docs/src/microsite/img"),
    micrositeCssDirectory                      := file("docs/src/microsite/styles"),
    micrositeSassDirectory                     := file("docs/src/microsite/partials"),
    micrositeJsDirectory                       := file("docs/src/microsite/scripts"),
    micrositeTheme                             := "light",
    micrositeHighlightLanguages ++= Seq("scala", "sbt"),
    micrositeConfigYaml                        := ConfigYml(
      yamlCustomProperties = Map(
        "url"     -> "https://vigoo.github.io",
        "plugins" -> List("jemoji", "jekyll-sitemap")
      )
    ),
    micrositeFooterText                        := Some(
      "<a href='https://thenounproject.com/search/?q=Evolution%20&i=2373364'>Evolution</a> by Nithinan Tatah from the Noun Project<br><a href='https://thenounproject.com/search/?q=floppy&i=303328'>Floppy</a> by Jonathan Li from the Noun Project"
    ),
    micrositeAnalyticsToken                    := "UA-56320875-2",
    makeSite / includeFilter                   := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.txt" | "*.xml" | "*.svg",
    micrositeGithubToken                       := sys.env.get("GITHUB_TOKEN"),
    micrositePushSiteWith                      := GitHub4s
  )
  .dependsOn(core.jvm, catsEffect.jvm, zio.jvm, zioPrelude.jvm, akka, cats.jvm, shapeless.jvm, shardcake, docsPlugins)

// Temporary fix to avoid including mdoc in the published POM

import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}
import scala.xml.transform.{RewriteRule, RuleTransformer}

// skip dependency elements with a scope
pomPostProcess := { (node: XmlNode) =>
  new RuleTransformer(new RewriteRule {
    override def transform(node: XmlNode): XmlNodeSeq = node match {
      case e: Elem
          if e.label == "dependency" && e.child
            .exists(child => child.label == "artifactId" && child.text.startsWith("mdoc_")) =>
        val organization = e.child.filter(_.label == "groupId").flatMap(_.text).mkString
        val artifact     = e.child.filter(_.label == "artifactId").flatMap(_.text).mkString
        val version      = e.child.filter(_.label == "version").flatMap(_.text).mkString
        Comment(s"dependency $organization#$artifact;$version has been omitted")
      case _ => node
    }
  }).transform(node).head
}
