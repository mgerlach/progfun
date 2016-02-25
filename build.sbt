name := "progfun"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.6.3",
  "de.idealo.offermanager" % "om-publisher-api" % "0.15.1",
  "de.idealo.services" % "services-core" % "0.26.1")
