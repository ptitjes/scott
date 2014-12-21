package io.github.ptitjes.scott.analysis

import java.io.{PrintWriter, File, FileWriter}

import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._

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
							"\\pgfplotscreateplotcyclelist{alllinestyles}" +
							"{solid,loosely dashed,dotted,dashdotted,dashdotdotted," +
							"densely dashed,densely dotted,densely dashdotted,densely dashdotdotted}\n" +
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

	case class YAxis[X](name: String, unit: String, f: (X, Results) => Double)

	abstract class Plot[X] extends ReportElement {

		def name: String

		def title: String

		def common: ConfigurationSet

		def makeHeader(out: PrintWriter, options: String) {
			val titleDetailsPart = common.flatten().toString
			val titleDetails =
				if (!titleDetailsPart.isEmpty) " (" + titleDetailsPart + ")"
				else ""

			out.println(
				"\\begin{tikzpicture}" +
					"\\begin{axis}[\n" +
					s"\ttitle=${title + titleDetails},\n" +
					options + "\n" +
					s"]")
		}

		def makeFooter(out: PrintWriter) {
			out.println(
				"\\end{axis}\n" +
					"\\end{tikzpicture}\n" +
					"\n")
		}

		def makeTable(out: PrintWriter, columns: List[Configuration], data: List[(Any, List[Double])]) = {
			out.println("\\pgfplotstableread{")
			out.println("x\t" + (0 until columns.length).map(i => s"y$i").mkString("\t"))
			data.foreach {
				case (r, values) => out.println(r + "\t" + values.mkString("\t"))
			}
			out.println(s"}\\$name")
			out.println()
		}

		def makePlots(out: PrintWriter, kind: String, columns: List[Configuration]) = {
			out.println((0 until columns.length).map(
				i => s"\\addplot+[$kind] table[x=x,y=y$i] {\\$name};").mkString("\n")
			)
		}

		def makeLegend(out: PrintWriter, columns: List[Configuration]) = {
			out.println("\\legend{")
			out.println(columns.mkString(",\n"))
			out.println("}")
		}
	}

	case class LinePlot[X](name: String, title: String,
	                       common: ConfigurationSet,
	                       lines: ConfigurationSet,
	                       xAxis: MultiValuedParameter[X],
	                       yAxis: YAxis[X]) extends Plot[X] {

		def generate(runner: AnalysisRunner, out: PrintWriter) = {

			val (columns, data) = runner.resultsFor(common, lines, xAxis, yAxis.f)

			makeTable(out, columns, data)

			def makeTicks = {
				val ticks = xAxis.values
				if (ticks.size <= 10) "\txtick={" + ticks.mkString(",") + "},\n"
				else ""
			}

			makeHeader(out,
				"\tlegend style={\n" +
					"\t\tcells={anchor=east},\n" +
					"\t\tlegend pos=outer north east,\n" +
					"\t},\n" +
					makeTicks +
					"\tcycle list name=mark list*,\n" +
					"\tcycle list name=alllinestyles, semithick,\n" +
					s"\txlabel=${xAxis.parameter.name},\n" +
					s"\tylabel=${yAxis.name} (${yAxis.unit})"
			)

			makeLegend(out, columns)

			makePlots(out, "smooth", columns)

			makeFooter(out)
		}
	}

	case class BarPlot[X](name: String, title: String,
	                      common: ConfigurationSet,
	                      bars: ConfigurationSet,
	                      xAxis: List[X],
	                      yAxis: YAxis[X]) extends Plot[X] {

		def generate(runner: AnalysisRunner, out: PrintWriter) = {

			val (columns, data) = runner.resultsFor(common, bars, xAxis, yAxis.f)

			makeTable(out, columns, data)

			makeHeader(out,
				"width=\\textwidth-\\widthof{100}-0.1cm,\n" +
					"\tlegend style={\n" +
					"\t\tat={(0.5,-0.15)},\n" +
					"\t\tanchor=north\n" +
					"\t},\n" +
					"\tybar, bar width=6pt, bar cycle list,\n" +
					"\tenlargelimits=0.05,\n" +
					s"\tylabel=${yAxis.name} (${yAxis.unit})"
			)

			makeLegend(out, columns)

			makePlots(out, "ybar", columns)

			makeFooter(out)
		}
	}

}
