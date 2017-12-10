resolvers in ThisBuild += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

organization in ThisBuild := "be.kuleuven"
scalaVersion in ThisBuild := "2.11.8"
version in ThisBuild := "0.0.2-SNAPSHOT"

EclipseKeys.withSource in ThisBuild := false
EclipseKeys.withJavadoc in ThisBuild := false

scalacOptions in ThisBuild ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-language:higherKinds"
)

lazy val proman = crossProject
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.6.7"
    ) ++ Seq("circe-core", "circe-generic", "circe-parser").map(
      "io.circe" %%% _ % "0.8.0"
    )
  )
  .jvmSettings(
    name := "jvm",
    libraryDependencies ++= Seq(
      "org.slf4j" % "slf4j-nop" % "1.6.4",
      "com.typesafe.slick" %% "slick" % "3.2.1"
    ) ++ Seq("http4s-blaze-server", "http4s-dsl", "http4s-circe").map(
      "org.http4s" %% _ % "0.17.5"
    )
  )
  .jsSettings(
    name := "js",
    scalaJSUseMainModuleInitializer := true
  )

// Needed, so sbt finds the projects
lazy val server = proman.jvm
  .settings(
    scalaJSProjects := Seq(client),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    managedClasspath in Runtime += (packageBin in Assets).value
).enablePlugins(SbtWeb)

lazy val client = proman.js.enablePlugins(ScalaJSPlugin, ScalaJSWeb)
