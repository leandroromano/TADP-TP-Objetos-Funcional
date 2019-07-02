
import org.scalatest.{FreeSpec, Matchers}

class MusicParserTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe(expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParseErrorExcetion](actualResult)
  }

  "Parsers" - {
    "Any Char" - {
      "parses a char" in {
        assertParsesSucceededWithResult(anyChar("hola").parse(), 'h')
      }

      "fails with empty string" - {
        "throws exception" in {
          assertThrows[ParseErrorException]("texto vacio")(anyChar("").parse())
        }
      }
    }

    "char" - {

        "it parses a the first char" in {
          assertParsesSucceededWithResult(char('h')("hola").parse(), 'h')
        }

        "it fails to find the char" in {
          assertThrows[ParseErrorException]("caracter incorrecto")(char('h')("chau").parse())
        }
    }


    "void" - {
      "it parses Unit" in {
        assertParsesSucceededWithResult(void("hola").parse(), Unit)
      }

      "it fails" in {
        assertThrows[ParseErrorException]("texto vacio")(void("chau").parse())
      }
    }

    "letter" - {
      "parses a letter" in {
        assertParsesSucceededWithResult(letter("h").parse(), 'h')
      }

      "it fails when fed a number" in {
        assertThrows[ParseErrorException]("no es una letra")(letter("1234").parse())
      }
    }

    "digit" - {
      "it fails when fed a letter" in {
        assertThrows[ParseErrorException]("no es digito")(digit("hola").parse())
      }

      "it parses a digit" in {
        assertParsesSucceededWithResult(digit("1234").parse(), 1)
      }
    }

    "alphaNum" - {
      "parses an alphanumeric character" in {
        assertParsesSucceededWithResult(alphaNum("h0l4").parse(), 'h')
      }

      "fails for not alphanumeric" in {
        assertThrows[ParseErrorException]("no es un caracter alfanumerico")(alphaNum("!").parse())
      }
    }

    "string" - {
      "parses a string character" in {
        assertParsesSucceededWithResult(string("no andan")("no andan los tests").parse(), "no andan")
      }

      "fails for not alphanumeric" in {
        assertThrows[ParseErrorException]("cadena incorrecta")(string("no andan")("si andan").parse())
      }
    }
  }

  "Combinators" - {


    "OR Combinator" - {
      val aob = char('a') <|> char('b')

      "parses with the first one" in {
        assertParsesSucceededWithResult(aob("aloha").parse(), 'a')
      }

      "parses with the second one" in {
        assertParsesSucceededWithResult(aob("bort").parse(), 'b')
      }

      "parser does not parse with either one" {
        assertThrows[ParseErrorException]("todos fallan")(aob("no").parse())
      }
    }

    "Concat Combinator" - {
      val holaMundo = string("hola") <> string("mundo")

      "parses" in {
        assertParsesSucceededWithResult(holaMundo("holamundo").parse(), ("hola", "mundo"))
      }
    }

    "Rightmost Combinator" - {
      val rightMost = digit ~> alphaNum

      "parses" in {
        assertParsesSucceededWithResult(holaMundo("1abc").parse(), 'a')
      }

      "does not parse" in {
        assertParsesSucceededWithResult(holaMundo("1abc").parse(), '1')
      }
    }

    "Leftmost Combinator" - {
      val rightMost = digit <~ alphaNum

      "parses" in {
        assertParsesSucceededWithResult(holaMundo("1abc").parse(), '1')
      }

      "does not parse" in {
        assertParsesSucceededWithResult(holaMundo("1abc").parse(), 'a')
      }
    }



  }
}

