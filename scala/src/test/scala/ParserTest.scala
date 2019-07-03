
import org.scalatest.{FreeSpec, Matchers}

class MusicParserTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe(expectedResult)
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
          assertParserFailed("texto vacio")(new anyChar().parse("").getResultado)
        }
      }
    }

    "char" - {

        "it parses a the first char" in {
          assertParsesSucceededWithResult(new char('h').parse("hola").getResultado, 'h')
        }

        "it fails to find the char" in {
          assertParserFailed("caracter incorrecto")(new char('h').parse("chau").getResultado)
        }
    }


    "void" - {
      "it parses Unit" in {
        assertParsesSucceededWithResult(new void().parse("hola").getResultado, Unit)
      }

      "it fails" in {
        assertParserFailed("texto vacio")(new void().parse("").getResultado)
      }
    }

    "letter" - {
      "parses a letter" in {
        assertParsesSucceededWithResult(new letter().parse("hola").getResultado, 'h')
      }

      "it fails when fed a number" in {
        assertParserFailed("no es una letra")(new letter().parse("123").getResultado)
      }
    }

    "digit" - {
      "it fails when fed a letter" in {
        assertParserFailed("no es digito")(new digit().parse("hola").getResultado)
      }

      "it parses a digit" in {
        assertParsesSucceededWithResult(new digit().parse("1234").getResultado, 1)
      }
    }

    "alphaNum" - {
      "parses an alphanumeric character" in {
        assertParsesSucceededWithResult(new alphaNum().parse("h0l4").getResultado, 'h')
      }

      "fails for not alphanumeric" in {
        assertParserFailed("no es un caracter alfanumerico")(new alphaNum().parse("!").getResultado)
      }
    }

    "string" - {
      "parses a string character" in {
        assertParsesSucceededWithResult(new string("no andan").parse("no andan los tests").getResultado, "no andan")
      }

      "fails for not alphanumeric" in {
        assertParserFailed("cadena incorrecta")(new string("no andan").parse("si andan").getResultado)
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
        assertParserFailed("todos fallan")(aob.parse("hola").getResultado)
      }
    }

    "Concat Combinator" - {
      val holaMundo = new string("hola") <> new string("mundo")

      "parses" in {
        assertParsesSucceededWithResult(holaMundo.parse("holamundo").getResultado, ("hola", "mundo"))
      }

      "fails" in {

      }
    }

    "Rightmost Combinator" - {
      val rightMost = new digit ~> new alphaNum

      "parses" in {
        assertParsesSucceededWithResult(rightMost.parse("4A").getResultado, 'A')
      }

      "fails" in {

      }
    }

    "Leftmost Combinator" - {
      val leftMost = new digit <~ new alphaNum

      "parses" in {
        assertParsesSucceededWithResult(leftMost.parse("4abc"), '4')
      }

      "fails" in {

      }
    }

  }
}

