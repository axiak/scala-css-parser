import annotation.tailrec
import collection.mutable.ArrayBuffer
import util.parsing.combinator._
import java.io.File

// See http://www.w3.org/TR/css3-syntax/#grammar0
class SimpleCSSParser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])+\*/)+""".r

  // Lexical symbols
  def h = "[0-9a-fA-F]".r
  def nonascii = "[\200-\377]"
  def unicode = "\\[0-9a-fA-F]{1,6}".r
  def escape = unicode | "\\[ -~\200-\377]".r
  def nmstart = "[a-zA-Z]" | nonascii | escape
  def nmchar = "[a-zA-Z0-9-]" | nonascii | escape
  override def stringLiteral = ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt"]|\\u[a-fA-F0-9]{4})*"""+"\"").r | ("\'"+"""([^'\p{Cntrl}\\]|\\[\\/bfnrt']|\\u[a-fA-F0-9]{4})*"""+"\'").r
  override def ident = """[*@_]?-?[a-zA-Z_][a-zA-Z0-9_-]*""".r
  def name = rep1(nmchar)
  def CDO = "<!--"
  def CDC = "-->"
  def INCLUDES = "~="
  def DASHMATCH = "|="

  def url = rep("[!#$%&*-~".r | nonascii | escape)
  def IMPORT_SYM = "(?i)@import".r
  def PAGE_SYM = "(?i)@page".r
  def MEDIA_SYM = "(?i)@media".r
  def FONT_FACE_SYM = "(?i)@font-face".r
  def CHARSET_SYM = "(?i)@charset".r
  def NAMESPACE_SYM = "(?i)@namespace".r
  def IMPORTANT_SYM = "!important" | ("!" ~ "important")
  def EMS = decimalNumber ~ "em"
  def EXS = decimalNumber ~ "ex"
  def RESOLUTION = decimalNumber ~ "(?i)dpi".r
  def LENGTH = decimalNumber ~ "(?i)(?:px|cm|mm|in|pt|pc)".r
  def ANGLE = decimalNumber ~ "(?i)(?:deg|rad|grad)".r
  def TIME = decimalNumber ~ "(?i)(?:s|ms)".r
  def FREQ = decimalNumber ~ "(?i)(?:Hz|kHz)".r
  def DIMEN = decimalNumber ~ ident
  def PERCENTAGE = decimalNumber ~ "%"
  def NUMBER = decimalNumber | "\\" ~ decimalNumber
  def URI = "url(" ~ ((stringLiteral | "[^)]+".r) ^^ (URL(_))) ~ ")"

  def hexcolor = "#(?:[0-9A-Fa-f]{3}){1,2}".r
  def function = "[a-zA-Z:._0-9-]+\\(".r ~ funcexpr ~ ")"
  def unary_operator = "-" | "+"
  def term: Parser[Any] = unary_operator | ((PERCENTAGE | LENGTH | EMS | EXS | ANGLE | RESOLUTION |
                          TIME | FREQ | URI | hexcolor | stringLiteral | NUMBER | ie_expression | function | ident) ^^ (NeedsSpace(_)))
  def expr = rep1(term ~ opt(operator))

  def ie_expression_no_paren = "[^\\(\\)]+".r
  def ie_expression_paren: Parser[Any] = "(" ~ rep(ie_expression_no_paren | ie_expression_paren) ~ ")"
  def ie_expression = "expression" ~ ie_expression_paren

  // This is an extension of the css spec to allow filter: alpha(opacity=xx) syntax (kwargs).
  def funcexpr = rep(opt(ident ~ "=") ~ term ~ opt(operator))
  def operator = "/" | ","
  def combinator = "+" | ">" | "~"
  def prio = IMPORTANT_SYM
  def declaration = property ~ ":" ~ expr ~ opt(prio)
  def transform_declaration = """(?i)(?:from|to)""".r ~ "{" ~ rep1(declaration ~ rep(";")) ~ "}"
  def pseudo = ":" ~ opt((ident ~ "(" ~ (HASH | class_ | ident) ~ ")") | ident)
  def attrib = "[" ~ ident ~ opt(opt("=" | INCLUDES | DASHMATCH) ~ (ident | stringLiteral)) ~ "]"
  def element_name = "*" | ident | "/**/"
  def class_ = "." ~ ident
  def HASH = "#" ~ ident
  def selector_modifier = HASH | class_ | attrib | pseudo
  def simple_selector = (element_name ~ rep(selector_modifier)) | (rep1(selector_modifier))
  def selector = simple_selector ~ opt(combinator | ",")
  def declaration_body = "{" ~ rep(transform_declaration | declaration ~ rep(";"))  ~ "}"
  def ruleset = rep1(selector ^^ (NeedsSpace(_))) ~ declaration_body
  def property = ident
  def font_face = FONT_FACE_SYM ~ declaration_body
  def moz_document = ("(?i)@-moz-document".r ^^ (NeedsSpace(_))) ~ opt(function) ~ "{" ~ rep(ruleset) ~ "}"
  def pseudo_page = ":" ~ ident
  def medium = ident
  def media_qualifier = "(" ~ ident ~ ":" ~ term ~ ")"
  def media_term = (ident | media_qualifier) ~ opt(",")
  def page = (PAGE_SYM ^^ (NeedsSpace(_))) ~ opt(ident) ~ opt(pseudo_page) ~ "{" ~ rep1sep(declaration, ",") ~ "}"
  def media = (MEDIA_SYM ^^ (NeedsSpace(_))) ~ rep1(media_term) ~ "{" ~ rep(ruleset) ~ "}"
  def namespace_prefix = ident
  def namespace = (NAMESPACE_SYM  ^^ (NeedsSpace(_)))  ~ opt(namespace_prefix) ~ opt(stringLiteral | URI) ~ ";"
  def import_ = (IMPORT_SYM ^^ (NeedsSpace(_))) ~ (stringLiteral | URI) ~ repsep(medium, ",") ~ ";"
  def stylesheet = opt((CHARSET_SYM^^ (NeedsSpace(_))) ~ stringLiteral ~ ";") ~
                   rep(import_) ~ rep(namespace)  ~
                   rep(media | page | font_face | moz_document | ruleset)
}

case class URL(url: String) {
  val AbsolutePattern = """^(?i)(?:/|(?:http|ftp|https|spdy)://).*""".r
  val InsideQuote = """^(['\"]?)(.+)\1$""".r

  def rewrite(prefix: String): String = url match {
    case InsideQuote(quote, content) => {
          quote + rewriteInside(content, prefix) + quote
        }
    case _ => rewriteInside(url, prefix)
  }

  private def rewriteInside(inside: String, prefix: String): String = inside match {
    case AbsolutePattern() => inside
    case _ => prefix + inside
  }
}


case class NeedsSpace(token: Any)


object Main extends SimpleCSSParser {
  var prefix: String = ""

  def main(args: Array[String]) {
    val noSpace = Set(";", "}", ")", "{", "(", ",", ">", "<", "+")
    val prefixIndex = args.zipWithIndex filter {case (arg, idx) => arg == "-t"}
    val target = if (prefixIndex.length > 0)
      Some(args(prefixIndex(0)._2 + 1))
    else
      None

    val input = if (args.length > 0 && args(0) != "-t") {
      if (target.isDefined) {
        val parent = new File(args(0)).getParent
        val sourceList = if (parent == null || parent == "")
          List()
        else
          parent.split(File.separator).toList
        this.prefix = computePrefix(sourceList, target.get.split(File.separator).toList).mkString("/")
        if (!this.prefix.isEmpty)
          this.prefix += "/"
      }
      io.Source.fromFile(args(0))
    } else {
      io.Source.stdin
    }
    val result = parseAll(stylesheet, input.getLines().mkString("\n"))
    try {
      val flatResult = flatResultList(result)
      // Beautify by removing needless spaces.
      flatResult.zipWithIndex foreach { case (value, idx) => if (idx > 0 && noSpace.contains(value) && flatResult(idx - 1) == " ") flatResult(idx - 1) = "" }
      print(flatResult.mkString(""))
    } catch {
      case e: Exception => println(result)
    }
  }

  @tailrec
  def computePrefix(sourceDir: List[String], target: List[String], acc: List[String] = List()): List[String] = (sourceDir, target) match {
    case (shead :: srest, thead :: trest) => if (shead == thead) computePrefix(srest, trest, acc) else computePrefix(srest, trest, ".." :: acc ::: List(shead))
    case (Nil, thead :: trest) => computePrefix(Nil, trest, ".." :: acc)
    case (shead :: srest, Nil) => computePrefix(srest, Nil, acc ::: List(shead))
    case (Nil, Nil) => acc
  }

  def flatResultList(result: Any): ArrayBuffer[String] = result match {
    case a: Some[Any] => flatResultList(a.get)
    case a: ParseResult[Any] => flatResultList(a.get)
    case a: ~[Any, Any] => flatResultList(a._1) ++ flatResultList(a._2)
    case a :: rest => flatResultList(a) ++ flatResultList(rest)
    case a: String => ArrayBuffer(a)
    case None => ArrayBuffer()
    case List() => ArrayBuffer()


    /* Put any rewrite rule here, and annotate the above tokens with ^^ to do it. */
    case url: URL => ArrayBuffer(url.rewrite(this.prefix))
    case needsSpace: NeedsSpace => flatResultList(needsSpace.token) ++ ArrayBuffer(" ")

  }
}

