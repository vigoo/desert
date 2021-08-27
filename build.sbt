import microsites.ConfigYml
import sbt.Keys.scalacOptions
import sbtcrossproject.{CrossProject, CrossType}
import scoverage.ScoverageKeys.coverageEnabled
import xerial.sbt.Sonatype._

name := "desert"

dynverSonatypeSnapshots in ThisBuild := true

lazy val commonSettings = Seq(
  organization := "io.github.vigoo",
  scalaVersion := "2.13.6",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

  libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % "1.0.11" % "test",
    "dev.zio" %% "zio-test" % "1.0.11" % "test",
    "dev.zio" %% "zio-test-sbt" % "1.0.11" % "test",
    "dev.zio" %% "zio-test-magnolia" % "1.0.11" % "test",
  ),

  coverageEnabled in(Test, compile) := true,
  coverageEnabled in(Compile, compile) := false,
  scalacOptions := Seq(
    "-deprecation",
    "-unchecked"
  ),

  // Publishing
  publishMavenStyle := true,
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  sonatypeProjectHosting := Some(GitHubHosting("vigoo", "desert", "daniel.vigovszky@gmail.com")),
  developers := List(
    Developer(id = "vigoo", name = "Daniel Vigovszky", email = "daniel.vigovszky@gmail.com", url = url("https://vigoo.github.io"))
  ),

  credentials ++=
    (for {
      username <- Option(System.getenv().get("SONATYPE_USERNAME"))
      password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
    } yield
      Credentials(
        "Sonatype Nexus Repository Manager",
        "oss.sonatype.org",
        username,
        password)).toSeq,

  resolvers +=
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

lazy val root = Project("desert", file("."))
  .settings(commonSettings)
  .settings(
  publishArtifact := false,
  description := "A Scala binary serialization library",
  )
  .aggregate(
    core.jvm, core.js,
    akka,
    cats.jvm, cats.js,
    catsEffect.jvm, catsEffect.js,
    zio.jvm, zio.js,
    benchmarks)

lazy val core = CrossProject("desert-core", file("desert-core"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "A Scala binary serialization library",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "dev.zio" %% "zio-prelude" % "1.0.0-RC6",      
      "com.chuusai" %% "shapeless" % "2.3.7",
    ),
  )
  .jsSettings(coverageEnabled := false)

lazy val akka = Project("desert-akka", file("desert-akka")).settings(commonSettings).settings(
  description := "Akka serialization bindings for desert",
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.6.16",
    "com.typesafe.akka" %% "akka-actor-typed" % "2.6.16",
  )
).dependsOn(core.jvm)

lazy val cats = CrossProject("desert-cats", file("desert-cats"))(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    description := "Desert serializers for cats data types",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.6.1",
      "dev.zio" %% "zio-interop-cats" % "3.1.1.0" % Test
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
      "org.typelevel" %% "cats-effect" % "3.2.3",
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
      "dev.zio" %% "zio" % "1.0.11"
    )
  )
  .jsSettings(coverageEnabled := false)
  .dependsOn(core)

lazy val benchmarks = project.in(file("benchmarks"))
  .settings(commonSettings)
  .settings(
    publishArtifact := false,
    coverageEnabled := false,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test" % "1.0.11",
      "dev.zio" %% "zio-test-magnolia" % "1.0.11",
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core.jvm)

lazy val docs = project
  .settings(commonSettings)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(SiteScaladocPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(MicrositesPlugin)
  .settings(
      name := "desert",
      description := "A Scala binary serialization library",
      publishArtifact := false,
      siteSubdirName in ScalaUnidoc := "api",
      addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
      unidocProjectFilter in ( ScalaUnidoc, unidoc ) := inAnyProject -- inProjects(core.js, catsEffect.js, zio.js, cats.js, benchmarks),
      git.remoteRepo := "git@github.com:vigoo/desert.git",
      micrositeUrl := "https://vigoo.github.io",
      micrositeBaseUrl := "/desert",
      micrositeHomepage := "https://vigoo.github.io/desert/",
      micrositeDocumentationUrl := "/desert/docs",
      micrositeAuthor := "Daniel Vigovszky",
      micrositeTwitterCreator := "@dvigovszky",
      micrositeGithubOwner := "vigoo",
      micrositeGithubRepo := "desert",
      micrositeGitterChannel := false,
      micrositeDataDirectory := file("docs/src/microsite/data"),
      micrositeStaticDirectory := file("docs/src/microsite/static"),
      micrositeImgDirectory := file("docs/src/microsite/img"),
      micrositeCssDirectory := file("docs/src/microsite/styles"),
      micrositeSassDirectory := file("docs/src/microsite/partials"),
      micrositeJsDirectory := file("docs/src/microsite/scripts"),
      micrositeTheme := "light",
      micrositeHighlightLanguages ++= Seq("scala", "sbt"),
      micrositeConfigYaml := ConfigYml(
        yamlCustomProperties = Map(
          "url" -> "https://vigoo.github.io",
          "plugins" -> List("jemoji", "jekyll-sitemap")
        )
      ),
      micrositeFooterText := Some("<a href='https://thenounproject.com/search/?q=Evolution%20&i=2373364'>Evolution</a> by Nithinan Tatah from the Noun Project<br><a href='https://thenounproject.com/search/?q=floppy&i=303328'>Floppy</a> by Jonathan Li from the Noun Project"),
      micrositeAnalyticsToken := "UA-56320875-2",
      includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.txt" | "*.xml" | "*.svg",
    )
  .dependsOn(core.jvm, catsEffect.jvm, zio.jvm, akka, cats.jvm)

// Temporary fix to avoid including mdoc in the published POM

import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}
import scala.xml.transform.{RewriteRule, RuleTransformer}

// skip dependency elements with a scope
pomPostProcess := { (node: XmlNode) =>
  new RuleTransformer(new RewriteRule {
    override def transform(node: XmlNode): XmlNodeSeq = node match {
      case e: Elem if e.label == "dependency" && e.child.exists(child => child.label == "artifactId" && child.text.startsWith("mdoc_")) =>
        val organization = e.child.filter(_.label == "groupId").flatMap(_.text).mkString
        val artifact = e.child.filter(_.label == "artifactId").flatMap(_.text).mkString
        val version = e.child.filter(_.label == "version").flatMap(_.text).mkString
        Comment(s"dependency $organization#$artifact;$version has been omitted")
      case _ => node
    }
  }).transform(node).head
}
