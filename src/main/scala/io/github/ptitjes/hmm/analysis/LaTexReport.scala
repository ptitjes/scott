package io.github.ptitjes.hmm.analysis

import java.io.{PrintWriter, File, FileWriter}

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._

import scala.collection._

class LaTexReport(reportFilename: String) {

	import LaTexReport._

	private var elements = mutable.ArrayBuffer[ReportElement]()

	def <<(element: ReportElement) = elements += element

	def generate(implicit runner: AnalysisRunner) = {

		val reportFile = new File(reportFilename)
		val reportDirectory = reportFile.getParentFile

		if (!reportDirectory.exists()) reportDirectory.mkdirs()
		if (reportFile.exists()) reportFile.delete()

		using(new FileWriter(reportFile, true)) {
			fileWriter => using(new PrintWriter(fileWriter)) {
				out =>
					out.println(
						"\\documentclass{article}\n" +
							"\n\\RequirePackage[a4paper,portrait]{geometry}\n" +
							"\\geometry{lmargin=2cm,rmargin=2cm,tmargin=2cm,bmargin=2cm}\n" +
							"\n% Encoding & Fonts\n" +
							"\\RequirePackage[utf8]{inputenc}\n\\RequirePackage[T1]{fontenc}\n\\RequirePackage{lmodern}\n" +
							"\n\\usepackage{tikz}\n" +
							"\\usepackage{pgfplots}\n" +
							"\\pgfplotsset{compat=1.9}\n" +
							"\\usetikzlibrary{plotmarks}\n" +
							"\n\\begin{document}\n\\selectcolormodel{gray}\n")

					elements foreach { e => e.generate(runner, out)}

					out.println(
						"\\end{document}")
			}
		}

		val builder = new ProcessBuilder(PATH_TO_PDFLATEX + "/pdflatex",
			reportFile.getName,
			"-output-format=pdf"
		)
		builder.directory(reportDirectory)
		builder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
		builder.redirectError(ProcessBuilder.Redirect.INHERIT)
		val proc = builder.start()
		if (proc.waitFor() != 0) throw new IllegalStateException()
	}
}

object LaTexReport {

	val PATH_TO_PDFLATEX = "/usr/bin"

	trait ReportElement {

		def generate(runner: AnalysisRunner, writer: PrintWriter)
	}

	case class XAxis(param: Parameter[_])

	case class YAxis(name: String, unit: String, f: Results => Double)

	case class Graph(name: String, title: String,
	                 configurations: ConfigurationSet,
	                 xAxis: XAxis,
	                 yAxis: YAxis) extends ReportElement {

		def generate(runner: AnalysisRunner, out: PrintWriter) = {

			val results = runner.resultsFor(configurations)

			val columns: List[Configuration] = results.buildColumns(configurations, xAxis.param)

			def makeTicks = {
				val ticks = configurations(xAxis.param)
				if (ticks.size <= 10) "\txtick={" + ticks.mkString(",") + "},\n"
				else ""
			}

			out.println(
				"\\begin{tikzpicture}" +
					"\\begin{axis}[\n\tlegend style={\n\t\tcells={anchor=east},\n\t\tlegend pos=outer north east,\n\t},\n" +
					"\tcycle list name=mark list*,\n" +
					s"\ttitle=$title,\n" +
					makeTicks +
					s"\txlabel=${xAxis.param.name},\n\tylabel=${yAxis.name} (${yAxis.unit})]")

			out.println("\\pgfplotstableread{")
			out.println("x\t" + (0 until columns.length).map(i => s"y$i").mkString("\t"))
			results.extractData(configurations, xAxis.param, columns, yAxis.f).foreach {
				case (r, values) => out.println(r + "\t" + values.mkString("\t"))
			}
			out.println("}\\data")

			out.println("\\legend{")
			out.println(columns.mkString(",\n"))
			out.println("}")

			out.println((0 until columns.length).map(i => s"\\addplot+[smooth] table[x=x,y=y$i] {\\data};").mkString("\n"))

			out.println("\\end{axis}\n\\end{tikzpicture}\n\n")
		}
	}

}
