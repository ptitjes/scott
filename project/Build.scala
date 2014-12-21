import sbt._
import Keys._

object Build extends Build {

	import Dependencies._

	val VERSION = "0.1"

	lazy val basicSettings = Seq(
		version := VERSION,
		homepage := Some(new URL("http://ptitjes.github.io/scott")),
		organization := "io.github.ptitjes",
		organizationHomepage := Some(new URL("http://ptitjes.github.io")),
		description := "A part-of-speech tagger",
		startYear := Some(2014),
		licenses := Seq("LGPL 3" -> new URL("http://www.gnu.org/licenses/lgpl.txt")),
		scalaVersion := "2.11.3",
		scalacOptions := Seq(
			"-feature",
			"-language:implicitConversions",
			"-language:postfixOps",
			"-language:existentials",
			"-unchecked",
			"-deprecation",
			"-encoding", "utf8",
			"-Ywarn-adapted-args"/*,
			"-Xmax-classfile-name", "100"*/
		)
	)

	lazy val root = Project(id = "scott",
		base = file(".")) aggregate(scottCore, scottNl)

	lazy val scottCore = Project(id = "scott-core", base = file("scott-core"))
		.settings(basicSettings: _*)
		.settings(
	    libraryDependencies ++=
		    compile(json4s)
		)

	lazy val scottNl = Project(id = "scott-nl", base = file("scott-nl"))
		.dependsOn(scottCore)
		.settings(basicSettings: _*)
		.settings(
	    libraryDependencies ++=
		    compile(json4s)
		)
}
