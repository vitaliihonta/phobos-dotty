val dottyVersion = "0.22.0-RC1"

ThisBuild / name := "phobos"

ThisBuild / scalaVersion := dottyVersion


lazy val commonDependencies =
  libraryDependencies ++= List(
    ("org.typelevel" %% "cats-core" % "2.0.0").withDottyCompat(dottyVersion),
    "com.fasterxml" % "aalto-xml" % "1.2.1",
    ("org.scalactic" %% "scalactic" % "3.0.8" % "test").withDottyCompat(dottyVersion),
    ("org.scalatest" %% "scalatest" % "3.0.8" % "test").withDottyCompat(dottyVersion),
  )

def configuration(id: String)(project: Project): Project =
  project.settings(
    moduleName := s"phobos-$id",
    sources in (Compile, doc) := List.empty,
    commonDependencies,
    scalacOptions ++= List(
      "-language:experimental.macros",
      "-Yindent-colons"
    )
  )

def phobosModule(id: String) =
  Project(id, file(s"modules/$id"))
    .configure(configuration(id))

lazy val core = phobosModule("core")
//lazy val derevo = phobosModule("derevo") dependsOn (core % "compile->compile;test->test")
//lazy val enumeratum = phobosModule("enumeratum") dependsOn (core % "compile->compile;test->test")
//lazy val akka = phobosModule("akka") dependsOn (core % "compile->compile;test->test")
//lazy val monix = phobosModule("monix") dependsOn (core % "compile->compile;test->test")
//lazy val fs2 = phobosModule("fs2") dependsOn (core % "compile->compile;test->test")

lazy val modules: List[ProjectReference] = List(core/*, akka, derevo, enumeratum, monix, fs2*/)


lazy val phobos = project
  .in(file("."))
  .settings(
    moduleName := "phobos",
    skip in publish := true
  )
  .aggregate(modules: _*)
