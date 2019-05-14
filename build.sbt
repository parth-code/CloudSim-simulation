name := "Test"

version := "0.1"

scalaVersion := "2.12.8"

version := "1.2.8"

// https://mvnrepository.com/artifact/ch.qos.logback/logback-classic
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

// https://mvnrepository.com/artifact/ch.qos.logback/logback-core
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.2.3"

// https://mvnrepository.com/artifact/org.slf4j/slf4j-api
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"

// https://mvnrepository.com/artifact/org.slf4j/slf4j-simple
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25" % Test

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

// https://mvnrepository.com/artifact/org.scalamock/scalamock
libraryDependencies += "org.scalamock" %% "scalamock" % "4.1.0" % Test

// https://mvnrepository.com/artifact/com.typesafe/config
libraryDependencies += "com.typesafe" % "config" % "1.3.3"

Compile / unmanagedJars := (baseDirectory.value ** "*.jar").classpath
