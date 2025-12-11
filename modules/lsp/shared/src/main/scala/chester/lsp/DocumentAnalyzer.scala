package chester.lsp

import scala.language.experimental.genericNumberLiterals

import chester.core.AST
import chester.error.{FileContent, Problem, SourceReader, renderDoc}
import chester.reader.{FileNameAndContent, Source}
import chester.tyck.{ElabRunner, SymbolIndex}
import chester.utils.doc.DocConf

case class DocumentAnalysis(ast: AST, ty: Option[AST], index: SymbolIndex)

object DocumentAnalyzer:
  private given DocConf = DocConf.Default

  def analyze(uri: String, content: String): Either[Seq[String], DocumentAnalysis] =
    val source = Source(FileNameAndContent(uri, content))
    ElabRunner.elaborateSource(source, parseAsFile = true) match
      case (Some(ast), ty, problems) if problems.isEmpty =>
        val index = SymbolIndex.fromAst(ast)
        Right(DocumentAnalysis(ast, ty, index))
      case (_, _, problems) =>
        if problems.nonEmpty then Left(renderProblems(problems, source))
        else Left(Seq("Elaboration did not produce a result"))

  private def renderProblems(problems: Seq[Problem], source: Source)(using DocConf): Seq[String] =
    val reader = source.readContent.toOption
      .map(content => SourceReader.fromFileContent(FileContent(content, source.offset)))
      .getOrElse(SourceReader.empty)
    problems.map(_.renderDoc(using summon[DocConf], reader).toString)
