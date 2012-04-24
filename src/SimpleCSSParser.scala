import util.parsing.combinator._

// See http://www.w3.org/TR/css3-syntax/#grammar0
class SimpleCSSParser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  // Lexical symbols
  def h = "[0-9a-fA-F]".r
  def nonascii = "[\200-\377]"
  def unicode = "\\[0-9a-fA-F]{1,6}".r
  def escape = unicode | "\\[ -~\200-\377]".r
  def nmstart = "[a-zA-Z]" | nonascii | escape
  def nmchar = "[a-zA-Z0-9-]" | nonascii | escape

  override def ident = "\\*?-?[a-zA-Z_][a-zA-Z0-9_-]*".r
  def name = rep1(nmchar)
  def CDO = "<!--"
  def CDC = "-->"
  def INCLUDES = "~="
  def DASHMATCH = "|="

  def url = rep("[!#$%&*-~".r | nonascii | escape)
  def IMPORT_SYM = "@import"
  def PAGE_SYM = "@page"
  def MEDIA_SYM = "@media"
  def FONT_FACE_SYM = "@font-face"
  def CHARSET_SYM = "@charset"
  def NAMESPACE_SYM = "@namescpae"
  def IMPORTANT_SYM = "!important" | ("!" ~ "important")
  def EMS = decimalNumber ~ "em"
  def EXS = decimalNumber ~ "ex"
  def LENGTH = decimalNumber ~ "(?:px|cm|mm|in|pt|pc)".r
  def ANGLE = decimalNumber ~ "(?:deg|rad|grad)".r
  def TIME = decimalNumber ~ "(?:s|ms)".r
  def FREQ = decimalNumber ~ "(?:Hz|kHz)".r
  def DIMEN = decimalNumber ~ ident
  def PERCENTAGE = decimalNumber ~ "%"
  def NUMBER = decimalNumber | "\\" ~ decimalNumber
  def URI = "url\\(.*?\\)".r ^^ (URL(_))
  override def stringLiteral = super.stringLiteral | "\\\\?'[^']*\\\\?'".r |  "\\\\?\"[^\"]*\\\\?\"".r

  def hexcolor = "#(?:[0-9A-Fa-f]{3}){1,2}".r
  def function = "[a-zA-Z:._0-9-]+\\(".r ~ funcexpr ~ ")"
  def unary_operator = "-" | "+"
  def term: Parser[Any] = (unary_operator | PERCENTAGE | LENGTH | EMS | EXS | ANGLE |
                          TIME | FREQ | URI | hexcolor | stringLiteral | NUMBER | function | ident) ^^ (NeedsSpace(_))
  def expr = rep1(term ~ opt(operator))

  // This is an extension of the css spec to allow filter: alpha(opacity=xx) syntax (kwargs).
  def funcexpr = rep(opt(ident ~ "=") ~ term ~ opt(operator))
  def operator = "/" | ","
  def combinator = "+" | ">" | "~"
  def prio = IMPORTANT_SYM
  def declaration = property ~ ":" ~ expr ~ opt(prio)
  def pseudo = ":" ~ opt((ident ~ "(" ~ (HASH | class_ | ident) ~ ")") | ident)
  def attrib = "[" ~ ident ~ opt(opt("=" | INCLUDES | DASHMATCH) ~ (ident | stringLiteral)) ~ "]"
  def element_name = "*" | ident
  def class_ = "." ~ ident
  def HASH = "#" ~ ident
  def selector_modifier = HASH | class_ | attrib | pseudo
  def simple_selector = (element_name ~ rep(selector_modifier)) | (rep1(selector_modifier))
  def selector = simple_selector ~ opt(combinator | ",")
  def declaration_body = "{" ~ repsep(declaration, rep1(";")) ~ rep(";")  ~ "}"
  def ruleset = rep1(selector) ~ declaration_body
  def property = ident
  def font_face = FONT_FACE_SYM ~ declaration_body
  def moz_document = ("@-moz-document" ^^ (NeedsSpace(_))) ~ opt(function) ~ "{" ~ rep(ruleset) ~ "}"
  def pseudo_page = ":" ~ ident
  def medium = ident
  def page = (PAGE_SYM ^^ (NeedsSpace(_))) ~ opt(ident) ~ opt(pseudo_page) ~ "{" ~ rep1sep(declaration, ",") ~ "}"
  def media = (MEDIA_SYM ^^ (NeedsSpace(_))) ~ rep1sep(medium, ",") ~ "{" ~ rep(ruleset) ~ "}"
  def namespace_prefix = ident
  def namespace = (NAMESPACE_SYM  ^^ (NeedsSpace(_)))  ~ opt(namespace_prefix) ~ opt(stringLiteral | URI) ~ ";"
  def import_ = (IMPORT_SYM ^^ (NeedsSpace(_))) ~ (stringLiteral | URI) ~ repsep(medium, ",") ~ ";"
  def stylesheet = opt((CHARSET_SYM^^ (NeedsSpace(_))) ~ stringLiteral ~ ";") ~
                   rep(import_) ~ rep(namespace)  ~
                   rep(media | page | font_face | moz_document | ruleset)


}

case class URL(url: String) {
  val UrlPattern = "url\\(['\"]*(.*?)['\"]*\\)".r
  val AbsolutePattern = """^(?:(?:http|ftp|https|spdy)://|/)""".r
  def rewrite: String = url match {
    case UrlPattern(innerUrl) => innerUrl match {
      case AbsolutePattern() => url
      case _ => "url('" + "/rewrite!" + innerUrl + "')"
    }
  }
}


case class NeedsSpace(token: Any)


object Main extends SimpleCSSParser {
  def main(args: Array[String]) {
    val result = parseAll(stylesheet, io.Source.stdin.getLines().mkString("\n"))
    print(flatResultList(result).mkString(""))
  }

  def flatResultList(result: Any): List[String] = result match {
    case a: Some[Any] => flatResultList(a.get)
    case a: ParseResult[Any] => flatResultList(a.get)
    case a: ~[Any, Any] => flatResultList(a._1) ++ flatResultList(a._2)
    case a :: rest => flatResultList(a) ++ flatResultList(rest)
    case a: String => List(a)
    case None => List()
    case List() => List()


    /* Put any rewrite rule here, and annotate the above tokens with ^^ to do it. */
    case url: URL => List(url.rewrite)
    case needsSpace: NeedsSpace => flatResultList(needsSpace.token) ++ List(" ")

  }
}

