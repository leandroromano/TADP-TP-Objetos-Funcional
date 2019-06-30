import scala.util.Try
import scala.List

class ParseErrorException(message: String) extends RuntimeException(message)

trait ParserOutput[T] {
    def getSobra: String
    def getResultado: T
    def isParserSuccess: Boolean
}

case class ParserSuccess[T](resultado: T, sobra: String) extends ParserOutput[T] {
    override def getResultado: T = resultado
    override def getSobra: String = sobra
    override def isParserSuccess: Boolean = true
}

case class ParserFailure[T](message: String) extends ParserOutput[T] {
    override def getResultado = throw new ParseErrorException(message)
    override def getSobra =  throw new ParseErrorException(message)
    override def isParserSuccess: Boolean = false
}

sealed trait Parser[T] extends ((String) => ParserOutput[T]) {
    // ---- Combinators ----

    def <|>(parser: Parser[T]): Parser[T] = {
        return  new GenericParser((entrada: String) => {
            val aux: (ParserOutput[T], ParserOutput[T]) = (this(entrada), parser(entrada))
            aux match {
                case po: (ParserSuccess[T], _) => po._1
                case po: (_, ParserSuccess[T]) => po._2
                case _                         => ParserFailure("todos fallan")
            }
        })
    }

    def <>[U](parser: Parser[U]): Parser[(ParserOutput[T], ParserOutput[U])] = {
        return new GenericParser((entrada: String) => {
            val first: ParserOutput[T] = this (entrada)
            first match {
                case po: ParserFailure[T] => ParserFailure(po.message) // si uso po no anda (no matchea por tipos) y no entiendo por qué
                case _ => {
                    val retorno: (ParserOutput[T], ParserOutput[U]) = (first, parser(first.getSobra))

                    retorno match {
                        case tupla: (_, ParserSuccess[T]) => ParserSuccess[(ParserOutput[T], ParserOutput[U] )] (tupla, tupla._2.getSobra)
                        case _ => ParserFailure("el segundo fallo")
                    }
                }
            }
        })
    }

    def ~>(parser: Parser[T]): Parser[T] = {
        return new GenericParser((entrada: String) => {
            val retorno: ParserOutput[T] = this(entrada)

            retorno match {
                case ParserFailure(_) => retorno
                case r                => parser(r.getSobra)
            }

        })
    }

    def <~(parser: Parser[T]): Parser[T] = {
        return new GenericParser((entrada: String) => {
            val first: ParserOutput[T] = this(entrada)
            first match {
                case pf: ParserFailure[T] => pf
                case _ => {
                    val second: ParserOutput[T] = parser(first.getSobra)
                    second match {
                        case ParserFailure(_) => ParserFailure("el segundo fallo")
                        case _                => first // <-- qué "sobra" devuelve?
                    }
                }
            }
        })
    }

//    def recursividad[S, R](parser: Parser[T], parserSeparador: Parser[S], parser_resultado: ParserOutput[R], sobra: String): List[T] = {
//        parser_resultado match {
//            case pf: ParserFailure[T] => List()
//            case _ => {
//                parser match {
//                    case _: Parser[T] => {
//                        val nuevo_resultado = parserSeparador(sobra)
//                        recursividad(parser, parserSeparador, nuevo_resultado, nuevo_resultado.getSobra)
//                    }
//                    case parser: Parser[S] => {
//                        val nuevo_resultado = parser(sobra)
//                        recursividad(parser, parserSeparador, nuevo_resultado, nuevo_resultado.getSobra)
//                    }
//                }
////                val nuevo_resultado = parser(sobra)
////                recursividad(parser, parserSeparador, nuevo_resultado, nuevo_resultado.getSobra)
//            }
//        }
//    }

    def sepBy(parser: Parser[T]): Parser[T] = {
        return new GenericParser((entrada: String) => {
            var retorno: ParserOutput[T] = this(entrada)
//            var sobra: String
//            retorno match {
//                case pf: ParserFailure[T] => ParserFailure(pf.message)
//                case _ => {
////                    var sobra: String = retorno.getSobra
////                    var parserVariable: Parser[T] = parser
//
//                    val resultado = recursividad(this, parser, retorno, sobra)
//                    ParserSuccess(resultado, sobra)
////                    while (retorno.isParserSuccess && sobra != "" ) {
////                        retorno = parserVariable(sobra)
////
////                        if(retorno.isParserSuccess)
////                            sobra = retorno.getSobra
////
////                        if (parserVariable == this)
////                            parserVariable = parser
////                        else
////                            parserVariable = this
////                    }
//                }
//            }

            if (retorno.isParserSuccess) {
                var sobra: String = retorno.getSobra
                var parserVariable: Parser[T] = parser

                while (retorno.isParserSuccess && sobra != "" ) {
                    retorno = parserVariable(sobra)

                    if(retorno.isParserSuccess)
                        sobra = retorno.getSobra

                    if (parserVariable == this)
                        parserVariable = parser
                    else
                        parserVariable = this
                }
            }

            retorno
        })
    }

    // ---- Operaciones ----

    type Condicion = String => Boolean
    def satisfies(condicion: Condicion): Parser[T] = {
        return new GenericParser((entrada: String) => {
            var retorno: ParserOutput[T] = this(entrada)
            retorno match {
                case _ if !condicion(entrada) => ParserFailure("no se cumple condicion de entrada")
                case r                        => r
            }
        })
    }

    def opt: Parser[T] = {
        return new GenericParser((entrada: String) => {
            var retorno: ParserOutput[T] = this(entrada)

            retorno match {
                case ParserSuccess(_, _) => retorno
//                case _                   => ParserSuccess(null, entrada)
            }
        })
    }

    def *(): Parser[List[T]] = {
        return new GenericParser[List[T]]((entrada: String) => {
            var retorno: List[ParserOutput[T]] = List()
            var output: ParserOutput[T] = this(entrada)

            output match {
                case ParserFailure(_) => ParserSuccess[List[T]](List[T](), entrada)
                case _ => {
                    while(output.isParserSuccess){
                        retorno = retorno ::: List[ParserOutput[T]](output)
                        output = this(output.getSobra)
                    }
                    ParserSuccess[List[T]]((retorno.map(_.getResultado)), retorno.last.getSobra)
                }
            }
        })
    }

    def +(): Parser[List[T]] =  {
        return new GenericParser((entrada: String) => {
            var retorno: List[ParserOutput[T]] = List()
            var output: ParserOutput[T] = this(entrada)

            output match {
                case ParserFailure(_) => ParserFailure("El parser no se pudo aplicar la primera vez")
                case _ => {
                    while(output.isParserSuccess) {
                        retorno = retorno ::: List[ParserOutput[T]](output)
                        output = this(output.getSobra)
                    }
                    ParserSuccess[List[T]]( retorno.map(_.getResultado), retorno.last.getSobra)
                }
            }
        })
    }

    // def const(): Parser = ???
    
     def map[S](transformacion: T => S)(parser: Parser[T]): Parser[S] = {
         return new GenericParser[S]((entrada: String) => {
             val old: ParserOutput[T] = this(entrada)

             old match {
                 case ParserSuccess(retorno, sobra) => ParserSuccess[S](transformacion(retorno), sobra)
                 case ParserFailure(mensaje) => ParserFailure[S](mensaje)
             }
         })
     }
}

class GenericParser[A](comportamiento: (String) => ParserOutput[A]) extends Parser[A] {
    def apply(entrada: String): ParserOutput[A] = {
        this.comportamiento(entrada)
    }
}

class anyChar() extends Parser[Char]{
    def apply(entrada: String): ParserOutput[Char] = {
        entrada.toList match {
            case List() => ParserFailure("texto vacio")
            case c :: s => ParserSuccess[Char](c, s.mkString)
        }
    }
}

class char(caracter: Char) extends Parser[Char] {
     def apply(entrada: String): ParserOutput[Char] = {
        entrada.toList match {
            case List()                  => ParserFailure("caracter no encontrado")
            case c :: s if c == caracter => ParserSuccess[Char](c, s.mkString)
            case c :: s                  => ParserFailure("caracter incorrecto")
        }
    }
}

class void() extends Parser[Unit] {
     def apply(entrada: String): ParserOutput[Unit] = {
        entrada match {
            case "" => ParserFailure[Unit]("texto vacio")
            case _  => ParserSuccess[Unit](None, entrada.tail)
        }
    }
}

class letter() extends Parser[Char] {
     def apply(entrada: String): ParserOutput[Char] = {
        entrada.toList match {
            case List()               => ParserFailure("texto vacio")
            case c :: s if c.isLetter => ParserSuccess[Char](c, s.mkString)
            case _                    => ParserFailure("no es una letra")
        }
    }
}

class digit() extends Parser[Char] {
    def apply(entrada: String): ParserOutput[Char] = {
        entrada.toList match {
            case List()              => ParserFailure("texto vacio")
            case c :: s if c.isDigit => ParserSuccess[Char](c, s.mkString)
            case _                   => ParserFailure("no es un digito")
        }
    }
}

class alphaNum() extends Parser[Char] {
    def apply(entrada: String): ParserOutput[Char] = {
        val aux: (ParserOutput[Char], ParserOutput[Char]) = (new letter()(entrada), new digit()(entrada))
        aux match {
            case (ParserSuccess(_, _), _) => aux._1
            case (_, ParserSuccess(_, _)) => aux._2
            case _                        => ParserFailure("no es un caracter alfanumerico")
        }
    }
}

class string(cadena: String) extends Parser[String] {
    def apply(entrada: String): ParserOutput[String] = {
        val tamañoDeCadena: Integer = cadena.length
        val cadenaAParsear: String = entrada.take(tamañoDeCadena)
        if (cadena == "")
            ParserFailure("cadena vacia")
        else if (cadenaAParsear == cadena)
            ParserSuccess[String](cadena, entrada.drop(tamañoDeCadena))
        else
            ParserFailure("cadena incorrecta")
    }
}