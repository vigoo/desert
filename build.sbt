name := "desert"
description := "A Scala binary serialization library"
organization := "io.github.vigoo"

dynverSonatypeSnapshots in ThisBuild := true

scalaVersion := "2.13.2"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.typelevel" %% "cats-core" % "2.0.0",
  "com.chuusai" %% "shapeless" % "2.3.3",

  "dev.zio" %% "zio" % "1.0.0-RC21" % "test",
  "dev.zio" %% "zio-test"     % "1.0.0-RC21" % "test",
  "dev.zio" %% "zio-test-sbt" % "1.0.0-RC21" % "test",
  "dev.zio" %% "zio-test-junit" % "1.0.0-RC21" % "test",
  "dev.zio" %% "zio-test-magnolia" % "1.0.0-RC21" % "test",
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

coverageEnabled in(Test, compile) := true
coverageEnabled in(Compile, compile) := false

scalacOptions := Seq(
  "-deprecation",
  "-unchecked"
)


// Publishing

publishMavenStyle := true

licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

publishTo := sonatypePublishTo.value

import microsites.ConfigYml
import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("vigoo", "desert", "daniel.vigovszky@gmail.com"))

developers := List(
  Developer(id="vigoo", name="Daniel Vigovszky", email="daniel.vigovszky@gmail.com", url=url("https://vigoo.github.io"))
)

credentials ++=
  (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield
    Credentials(
      "Sonatype Nexus Repository Manager",
      "oss.sonatype.org",
      username,
      password)).toSeq

enablePlugins(GhpagesPlugin)
enablePlugins(SiteScaladocPlugin)
enablePlugins(MicrositesPlugin)

git.remoteRepo := "git@github.com:vigoo/desert.git"

micrositeUrl := "https://vigoo.github.io"
micrositeBaseUrl := "/desert"
micrositeHomepage := "https://vigoo.github.io/desert/"
micrositeDocumentationUrl := "/desert/docs"
micrositeAuthor := "Daniel Vigovszky"
micrositeTwitterCreator := "@dvigovszky"
micrositeGithubOwner := "vigoo"
micrositeGithubRepo := "desert"
micrositeGitterChannel := false
micrositeDataDirectory := file("src/microsite/data")
micrositeStaticDirectory := file("src/microsite/static")
micrositeImgDirectory := file("src/microsite/img")
micrositeCssDirectory := file("src/microsite/styles")
micrositeSassDirectory := file("src/microsite/partials")
micrositeJsDirectory := file("src/microsite/scripts")
micrositeTheme := "light"
micrositeHighlightLanguages ++= Seq("scala", "sbt")
micrositeConfigYaml := ConfigYml(
  yamlCustomProperties = Map("plugins" -> List("jemoji"))
)

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
