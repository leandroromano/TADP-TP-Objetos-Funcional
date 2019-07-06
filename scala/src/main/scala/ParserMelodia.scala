import Parsers._
import Musica._
import scala.util._

package object ParserMelodia {

    // SILENCIOS

    case class ParserSilencio() extends Parser[Silencio] {
        def apply(entrada: String): ParserOutput[Silencio] = {
            val pureParser: Parser[Char] = new char('-') <|> new char('_') <|> new char('~')
            val pureResult: ParserOutput[Char] = pureParser(entrada)

            pureResult match {
                case ParserSuccess('-', sobra) => ParserSuccess[Silencio](Silencio(Negra), sobra)
                case ParserSuccess('_', sobra) => ParserSuccess[Silencio](Silencio(Blanca), sobra)
                case ParserSuccess('~', sobra) => ParserSuccess[Silencio](Silencio(Corchea), sobra)
                case _                         => ParserFailure[Silencio]("no corresponde con ningun tipo de silencio")
            }
        }
    }

    // SONIDOS
    case class ParserFigura() extends Parser[Figura]{
        def apply(entrada: String): ParserOutput[Figura] = {
            val pureParser: Parser[String] =
                new string("1/1") <|>
                  new string("1/2") <|>
                    new string("1/4") <|>
                      new string("1/8") <|>
                        new string("1/16")

            val pureResult: ParserOutput[String] = pureParser(entrada)

            pureResult match {
                case ParserSuccess("1/1", sobra)  =>  ParserSuccess[Figura](Redonda, sobra)
                case ParserSuccess("1/2", sobra)  =>  ParserSuccess[Figura](Blanca, sobra)
                case ParserSuccess("1/4", sobra)  =>  ParserSuccess[Figura](Negra, sobra)
                case ParserSuccess("1/8", sobra)  =>  ParserSuccess[Figura](Corchea, sobra)
                case ParserSuccess("1/16", sobra) =>  ParserSuccess[Figura](SemiCorchea, sobra)
                case _                            => ParserFailure[Figura]("no es una figura")
            }
        }
    }

    case class ParserNota() extends Parser[Nota]{  // <-- ACA FALLA
        def apply(entrada: String): ParserOutput[Nota] = {
            val pureParser: Parser[(Char, Char)] =
                new letter() <> (new char('#') <|> new char('b')).opt

            val pureResult: ParserOutput[(Char, Char)] = pureParser(entrada)

            pureResult match {
                case ParserSuccess((posibleNota, modificador), sobra) if Nota.notas.map(_.toString).contains(posibleNota.toString) => {
                    val trueNote = Nota.notas(Nota.notas.map(_.toString).indexOf(posibleNota.toString))
                    val tryModificador: Try[Char] = Try(modificador) // <-- INTENTO DESESPERADO (?

                    tryModificador match {
                        case Success('b') => ParserSuccess[Nota](trueNote.bemol, sobra)
                        case Success('#') => ParserSuccess[Nota](trueNote.sostenido, sobra)
                        case Failure(_)   => ParserSuccess[Nota](trueNote, sobra)
                    }
                }
                case _ => ParserFailure[Nota]("no es una nota")
            }
        }
    }

    case class ParserTono() extends Parser[Tono] {
        def apply(entrada: String): ParserOutput[Tono] = {
            val pureParser: Parser[(Char, Nota)] =   new digit() <> new ParserNota()
            val pureResult: ParserOutput[(Char, Nota)] = pureParser(entrada)

            pureResult match {
                case ParserSuccess((octava, nota), sobra) => ParserSuccess[Tono](Tono(octava.asDigit, nota), sobra)
                case _ => ParserFailure[Tono]("no es un tono")
            }
        }
    }

    case class ParserSonido() extends Parser[Sonido] {
        def apply(entrada: String): ParserOutput[Sonido] = {
            val pureParser: Parser[(Tono, Figura)] = ParserTono() <> ParserFigura()
            val pureResult: ParserOutput[(Tono, Figura)] = pureParser(entrada)

            pureResult match {
                case ParserSuccess((tono, figura), sobra) => ParserSuccess[Sonido](Sonido(tono, figura), sobra)
                case _                                    => ParserFailure[Sonido]("no es un sonido")
            }
        }
    }

    //ACORDES

    case class ParserExplicitChord() extends Parser[Acorde] {
        def apply(entrada: String): ParserOutput[Acorde] = {
            val pureParser: Parser[(List[Tono], Figura)] = ParserTono().sepBy(new char('+')) <> ParserFigura()
            val pureResult: ParserOutput[(List[Tono], Figura)] = pureParser(entrada)

            pureResult match {
                case ParserSuccess((listaDeTonos, figura), sobra) => ParserSuccess[Acorde](Acorde(listaDeTonos, figura), sobra)
                case _                                            => ParserFailure[Acorde]("no es un acorde explicito")
            }
        }
    }

    case class ParserImplicitChord() extends Parser[Acorde] {
        def apply(entrada: String): ParserOutput[Acorde] = {
            val pureParser: Parser[((Tono, Char), Figura)] =
                (ParserTono() <> (new char('m') <|> new char('M'))) <> ParserFigura()
            val pureResult: ParserOutput[((Tono, Char), Figura)] = pureParser(entrada)

            pureResult match {
                case ParserSuccess(((tono, 'm'), figura), sobra) =>
                    ParserSuccess[Acorde](tono.nota.acordeMenor(tono.octava, figura), sobra)

                case ParserSuccess(((tono, 'M'), figura), sobra) =>
                    ParserSuccess[Acorde](tono.nota.acordeMayor(tono.octava, figura), sobra)

                case _                                           =>
                    ParserFailure[Acorde]("no es un acorde implicito")

            }
        }
    }

    case class ParserAcorde() extends Parser[Acorde] {
        def apply(entrada: String): ParserOutput[Acorde] = {
            val pureParser: Parser[Acorde] = ParserImplicitChord() <|> ParserExplicitChord()
            val pureReturn: ParserOutput[Acorde] = pureParser(entrada)

            pureReturn match {
                case ParserFailure(_) => ParserFailure[Acorde]("no es un acorde")
                case success => success
            }
        }
    }


    // MELODIA!!!


    // type Melodia = List(Tocable)      Tocable = Silencio / Sonido / Acorde
    //
    /*
    case object ParserMelodia extends Parser[Melodia] {
        def apply(entrada: String): ParserOutput[Melodia] = {
            val pureParser: Parser[Melodia] =
                (ParserSilencio() <|>
                    ParserSonido() <|>
                        ParserAcorde()).sepBy(new char(' '))

            pureParser(entrada)
        }
    }
*/
}
