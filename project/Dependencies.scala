import sbt._

object Dependencies {

	val json4s = "org.json4s" %% "json4s-native" % "3.2.11"


	def compile(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")

	def provided(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "provided")

	def test(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")

	def runtime(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "runtime")

	def container(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "container")
}