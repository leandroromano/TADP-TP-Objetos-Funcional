
import org.scalatest.{FreeSpec, Matchers}
import Parsers._
import ParserMelodia._
import Musica._

class ParserTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe expectedResult
  }

  def assertParserFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParseErrorException](actualResult)
  }

  "Parsers" - {
    "Any Char" - {
      "parses a char" in {
        assertParsesSucceededWithResult(new anyChar().parse("hola").getResultado, 'h')
      }

      "fails with empty string" - {
        "throws exception" in {
          assertParserFailed(new anyChar().parse("").getResultado)
        }
      }
    }

    "char" - {

        "it parses a the first char" in {
          assertParsesSucceededWithResult(new char('h').parse("hola").getResultado, 'h')
        }

        "it fails to find the char" in {
          assertParserFailed(new char('h').parse("chau").getResultado)
        }
    }


    "void" - {
      "it parses Unit" in {
        assertParsesSucceededWithResult(new void().parse("hola").getResultado, ())
      }

      "it fails" in {
        assertParserFailed(new void().parse("").getResultado)
      }
    }

    "letter" - {
      "parses a letter" in {
        assertParsesSucceededWithResult(new letter().parse("hola").getResultado, 'h')
      }

      "it fails when fed a number" in {
        assertParserFailed(new letter().parse("123").getResultado)
      }
    }

    "digit" - {
      "it parses a digit" in {
        assertParsesSucceededWithResult(new digit().parse("1234").getResultado, '1')
      }

      "it fails when fed a letter" in {
        assertParserFailed(new digit().parse("hola").getResultado)
      }
    }

    "alphaNum" - {
      "parses an alphanumeric character" in {
        assertParsesSucceededWithResult(new alphaNum().parse("h0l4").getResultado, 'h')
      }

      "fails for not alphanumeric" in {
        assertParserFailed(new alphaNum().parse("!").getResultado)
      }
    }

    "string" - {
      "parses a string character" in {
        assertParsesSucceededWithResult(new string("no andan").parse("no andan los tests").getResultado, "no andan")
      }

      "fails for not alphanumeric" in {
        assertParserFailed(new string("no andan").parse("si andan").getResultado)
      }
    }

  }

  "Combinators" - {


    "OR Combinator" - {
      val aob = new char('a') <|> new char('b')

      "parses with the first one" in {
        assertParsesSucceededWithResult(aob.parse("aloha").getResultado, 'a')
      }

      "parses with the second one" in {
        assertParsesSucceededWithResult(aob.parse("bort").getResultado, 'b')
      }

      "parser does not parse with either one" in {
        assertParserFailed(aob.parse("hola").getResultado)
      }
    }

    "Concat Combinator" - {
      val holaMundo = new string("hola") <> new string("mundo")

      "parses" in {
        assertParsesSucceededWithResult(holaMundo.parse("holamundo").getResultado, ("hola", "mundo"))
      }

      "fails" in {
        assertParserFailed(holaMundo.parse("chaumundo").getResultado)
      }
    }

    "Rightmost Combinator" - {
      val rightMost = new digit ~> new alphaNum

      "parses" in {
        assertParsesSucceededWithResult(rightMost.parse("4A").getResultado, 'A')
      }

      "fails" in {
        assertParserFailed(rightMost.parse("a23b").getResultado)
      }
    }

    "Leftmost Combinator" - {
      val leftMost = new digit <~ new alphaNum

      "parses" in {
        assertParsesSucceededWithResult(leftMost.parse("4abc").getResultado, '4')
      }

      "fails" in {
        assertParserFailed(leftMost.parse("a23b").getResultado)
      }
    }

    "satifies" - {
      val mayoresA5 = new digit satisfies (p => p.toInt > 5)
      "parses when condition is met" in {
        assertParsesSucceededWithResult(mayoresA5.parse("6").getResultado, '6')
      }

      "fails when condition is not met" in {
        assertParserFailed(mayoresA5.parse("4").getResultado)
      }
    }

    "opt" - {
      val talVezIn = string("in").opt
      val precedencia = talVezIn <> string("fija")

      "ignores the optional parse" in {
        assertParsesSucceededWithResult(precedencia.parse("fija").getResultado, (None, "fija"))
      }

      "parses with the optional" in {
        assertParsesSucceededWithResult(precedencia.parse("infija").getResultado, (Some("in"), "fija"))
      }
    }

    "* - kleene" - {
      val infiniteDigits = digit().*()

      "parses all digits" in {
        assertParsesSucceededWithResult(infiniteDigits.parse("1234").getResultado, List('1', '2', '3', '4'))
      }

      "gets an empty list if not found" in {
        assertParsesSucceededWithResult(infiniteDigits.parse("hola").getResultado, List())
      }
    }

    "+ - positive" - {
      val infiniteDigits = digit().+()

      "parses all digits" in {
        assertParsesSucceededWithResult(infiniteDigits.parse("1234").getResultado, List('1', '2', '3', '4'))
      }

      "gets at least one" in {
        assertParsesSucceededWithResult(infiniteDigits.parse("1hola").getResultado, List('1'))
      }
    }

    "sepBy" - {

      val numeroSeparadoPorGuion = digit().sepBy(char('-'))

      "parses respecting the separator" in {
        assertParsesSucceededWithResult(numeroSeparadoPorGuion .parse("1-1").getResultado, List('1','1'))
      }

      "fails if separator not found" in {
        assertParsesSucceededWithResult(numeroSeparadoPorGuion .parse("4 4").getResultado, List('4'))
      }
    }

    "const" - {
      val trueParser = string("true").const(true)

      "parses overriding the parsed" in {
        assertParsesSucceededWithResult(trueParser.parse("true").getResultado, true)
      }
    }

     "map" - {
       type String = List[Char]
       case class Punto(nombre: Int, apellido: Int)
       val personaParser = (digit() <> (char(';') ~> digit()))
         .map { case (x, y) => Punto(x.asDigit, y.asDigit) }

       "makes a Point" in {
         assertParsesSucceededWithResult(personaParser.parse("1;2").getResultado, Punto(1,2))

       }


     }
  }

  "Musiquita" - {

    "ParserSilencio" - {

      "parsea correctamente un silencio de blanca" in {
        assertParsesSucceededWithResult(ParserSilencio().parse("_").getResultado, Silencio(Blanca))
      }

      "parsea correctamente un silencio de negra" in {
        assertParsesSucceededWithResult(ParserSilencio().parse("-").getResultado, Silencio(Negra))
      }

      "parsea correctamente un silencio de corchea" in {
        assertParsesSucceededWithResult(ParserSilencio().parse("~").getResultado, Silencio(Corchea))
      }

      "falla cuando trata de parsear algo que no es un silencio" in {
        assertParserFailed(ParserSilencio().parse("no soy un silencio").getResultado)
      }

    }

    "ParserSonido" - {

      "parsea correctamente un sonido con semitono" in {
        assertParsesSucceededWithResult(ParserSonido().parse("6A#1/4").getResultado, Sonido(Tono(6, As), Negra))
      }

      "parsea correctamente un sonido sin semitono" in {
        assertParsesSucceededWithResult(ParserSonido().parse("6A1/4").getResultado, Sonido(Tono(6, A), Negra))
      }

      "falla cuando parsea algo que no es un sonido" in {
        assertParserFailed(ParserSonido().parse("no soy un sonido").getResultado)
      }

    }

    "ParserAcorde" - {

      "parsea correctamente un acorde explícito" in {
        assertParsesSucceededWithResult(ParserAcorde().parse("6A+6C#+6G1/8").getResultado, Acorde(List(Tono(6, A), Tono(6, Cs), Tono(6, G)), Corchea))
      }

      "parsea correctamente un acorde implicito" in {
        assertParsesSucceededWithResult(ParserAcorde().parse("6AM1/2").getResultado, A.acordeMayor(6, Blanca))
      }
    }

  }
}

