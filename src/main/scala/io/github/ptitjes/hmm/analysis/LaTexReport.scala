package io.github.ptitjes.hmm.analysis

import java.io.{PrintWriter, File, FileWriter}

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._

object LaTexReport {

  val PATH_TO_PDFLATEX = "/usr/bin"

  val REPORT_DIRECTORY = "report"
  val REPORT_FILENAME = "report.tex"

  trait ReportElement {

    def generate(results: ResultPool, writer: PrintWriter)
  }

  case class Graph(analysis: Analysis,
                   rows: Parameter[_],
                   columns: List[Configuration]) extends ReportElement {
    def generate(results: ResultPool, out: PrintWriter) = {

      out.println(
        "\\begin{tikzpicture}" +
          "\\begin{axis}[\n\tlegend style={\n\t\tcells={anchor=east},\n\t\tlegend pos=outer north east,\n\t},\n\txlabel=Un,\n\tylabel=Accuracy]")

      out.println("\\pgfplotstableread{")
      out.println("x\t" + (0 until columns.length).map(i => s"y$i").mkString("\t"))
      results.extractData(analysis, rows, columns).foreach {
        case (r, values) => out.println(r + "\t" + values.mkString("\t"))
      }
      out.println("}\\data")

      out.println("\\legend{")
      out.println(columns.mkString(",\n"))
      out.println("}")

      out.println((0 until columns.length).map(i => s"\\addplot+[smooth] table[x=x,y=y$i] {\\data};").mkString("\n"))

      out.println("\\end{axis}\n\\end{tikzpicture}")
    }
  }

  def generate(results: ResultPool)(elements: ReportElement*) = {

    val reportDirectory = new File(REPORT_DIRECTORY)
    if (!reportDirectory.exists()) reportDirectory.mkdirs()

    val file = new File(reportDirectory, REPORT_FILENAME)
    if (file.exists()) file.delete()

    using(new FileWriter(file, true)) {
      fileWriter => using(new PrintWriter(fileWriter)) {
        out =>
          out.println(
            "\\documentclass{article}\n" +
              "\\usepackage{tikz}\n" +
              "\\usepackage{pgfplots}\n" +
              "\\pgfplotsset{compat=1.9}\n" +
              "\\begin{document}\n")

          elements foreach { e => e.generate(results, out)}

          out.println(
            "\\end{document}")
      }
    }

    val builder = new ProcessBuilder(PATH_TO_PDFLATEX + "/pdflatex",
      REPORT_FILENAME,
      "-output-format=pdf"
    )
    builder.directory(reportDirectory)
    builder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    builder.redirectError(ProcessBuilder.Redirect.INHERIT)
    val proc = builder.start()
    if (proc.waitFor() != 0) throw new IllegalStateException()
  }

}
