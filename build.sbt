name := "andes"

version := "0.1"

scalaVersion := "2.12.2"

libraryDependencies  ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.14",
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.scala-graph" %% "graph-core" % "1.11.5",
  "com.lihaoyi" % "ammonite" % "1.0.1" % "test" cross CrossVersion.full,
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

(fullClasspath in Test) ++= {
  (updateClassifiers in Test).value
    .configurations
    .find(_.configuration == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect{case (a, f) if a.classifier == Some("sources") => f}
}
