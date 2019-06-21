import scala.util.Try
import scala.List

class ParseErrorException(message: String) extends RuntimeException(message)

trait ParserOutput {
    def isParserSuccess: Boolean
    def getSobra: String
    // def getResultado: Option[T] ??? -> Preguntar
}

case class ParserSuccess[T](resultado: Option[T], sobra: String) extends ParserOutput {
    def getResultado: Option[T] = resultado
    def getSobra: String = sobra
    def isParserSuccess: Boolean = true
}

case class ParserFailure(message: String) extends ParserOutput {
    def getResultado = throw new ParseErrorException(message)
    def getSobra =  throw new ParseErrorException(message)
    def isParserSuccess: Boolean = false
}

sealed trait Parser extends ((String) => ParserOutput) {
    // ---- Combinators ----

    def <|>(parser: Parser): Parser = {
        return  new GenericParser(((entrada: String) => {
            val aux: (ParserOutput, ParserOutput) = (this(entrada), parser(entrada))
            aux match{
                case (output1, _) if output1.isParserSuccess => output1
                case (_, output2) if output2.isParserSuccess => output2
                case _                                       => ParserFailure("todos fallan")  
            }
        }))
    }

    def <>(parser: Parser): Parser = {
        return new GenericParser(((entrada: String) => {
            val first: ParserOutput = this(entrada)
            if(!first.isParserSuccess)
                ParserFailure("el primero fallo")
            else {
                val retorno: (ParserOutput, ParserOutput) = (first, parser(first.getSobra))
                
                retorno match {
                    case (fst, snd) if (snd.isParserSuccess) =>
                        ParserSuccess[(ParserOutput, ParserOutput)]( Option((fst, snd)), snd.getSobra )
                    case _                                   => ParserFailure("el segundo fallo")
                }
            }
        }))
    }

    def ~>(parser: Parser): Parser = {
        return new GenericParser(((entrada: String) => {
            var retorno: ParserOutput = this(entrada)
        
            retorno match {
                case r if !r.isParserSuccess => retorno
                case r                       => parser(r.getSobra)
            }

        }))
    }

    def <~(parser: Parser): Parser = {
        return new GenericParser(((entrada: String) => {
            var first: ParserOutput = this(entrada)
            if (!first.isParserSuccess)
                ParserFailure("el primero fallo")
            else {
                val second: ParserOutput = parser(first.getSobra)

                second match {
                    case snd if (!snd.isParserSuccess) => ParserFailure("el segundo fallo")
                    case _                             => first // <-- qué "sobra" devuelve?
                }
            }
        }))
    }

    def sepBy(parser: Parser): Parser = {
        return new GenericParser(((entrada: String) => {
            var retorno: ParserOutput = this(entrada)

            if (retorno.isParserSuccess) {
                var sobra: String = retorno.getSobra
                var parserVariable: Parser = parser

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
        }))
    }

    // ---- Operaciones ----

    type Condicion = String => Boolean
    def satisfies(condicion: Condicion): Parser = {
        return new GenericParser(((entrada: String) => {
            var retorno: ParserOutput = this(entrada)
            retorno match {
                case _ if !condicion(entrada) => ParserFailure("no se cumple condicion de entrada")
                case r                        => r
            }
        }))
    }

    def opt: Parser = {
        return new GenericParser(((entrada: String) => {
            var retorno: ParserOutput = this(entrada)

            retorno match {
                case r if r.isParserSuccess => r
                case _                      => ParserSuccess(None, entrada)
            }
        }))
    }

    def *(): Parser = {
        return new GenericParser(((entrada: String) => {
            var retorno: List[ParserOutput] = List()
            var output: ParserOutput = this(entrada)

            if (!output.isParserSuccess)
                ParserSuccess[List[ParserOutput]](Option[List[ParserOutput]](List()), entrada)
            else {
                while(output.isParserSuccess){
                    retorno = retorno ::: List[ParserOutput](output)
                    output = this(output.getSobra)
                }
                ParserSuccess(Option[List[ParserOutput]](retorno), retorno.last.getSobra)
            }    
        }))
    }

    def +(): Parser =  {
        return new GenericParser(((entrada: String) => {
            var retorno: List[ParserOutput] = List()
            var output: ParserOutput = this(entrada)

            if (!output.isParserSuccess)
                output
            else {
                while(output.isParserSuccess){
                    retorno = retorno ::: List[ParserOutput](output)
                    output = this(output.getSobra)
                }
                ParserSuccess(Option[List[ParserOutput]](retorno), retorno.last.getSobra)
            }
        }))
    }

    // def const(): Parser = ???
    
    // def map(transformacion: T => A)(parser: Parser): Parser = {
    //     return new GenericParser(((entrada: String) => {
    //         val old: ParserOutput = this(entrada)

    //         old match {
    //             case ParserSuccess[T](Option[T](retorno), sobra) => ParserSuccess[A](Option[A](transformacion(retorno)), sobra)
    //             case _ => old
    //         }
    //     }))
    // }
}

class GenericParser(comportamiento: (String) => ParserOutput) extends Parser{
    def apply(entrada: String): ParserOutput = {
        this.comportamiento(entrada)
    }
}

class anyChar() extends Parser{
    def apply(entrada: String): ParserOutput = {
        entrada.toList match {
            case List() => ParserFailure("texto vacio")
            case c :: s => ParserSuccess[Char](Some(c), s.mkString)
        }
    }
}

class char(caracter: Char) extends Parser{
     def apply(entrada: String): ParserOutput = {
        entrada.toList match {
            case List()                  => ParserFailure("caracter no encontrado")
            case c :: s if c == caracter => ParserSuccess[Char](Some(c), s.mkString)
            case c :: s                  => ParserFailure("caracter incorrecto")
        }
    }
}

class void() extends Parser{
     def apply(entrada: String): ParserOutput = {
        entrada match {
            case "" => ParserFailure("texto vacio")
            case _  => ParserSuccess[Char](None, entrada.tail)
        }
    }
}

class letter() extends Parser{
     def apply(entrada: String): ParserOutput = {
        entrada.toList match {
            case List()               => ParserFailure("texto vacio")
            case c :: s if c.isLetter => ParserSuccess[Char](Some(c), s.mkString)
            case _                    => ParserFailure("no es una letra")
        }
    }
}

class digit() extends Parser{
    def apply(entrada: String): ParserOutput = {
        entrada.toList match {
            case List()              => ParserFailure("texto vacio")
            case c :: s if c.isDigit => ParserSuccess[Char](Some(c), s.mkString)
            case _                   => ParserFailure("no es un digito")
        }
    }
}

class alphaNum() extends Parser{
    def apply(entrada: String): ParserOutput = { 
        var aux: (ParserOutput, ParserOutput) = (new letter()(entrada), new digit()(entrada))
        aux match {
            case (output1, _) if output1.isParserSuccess => aux._1
            case (_, output2) if output2.isParserSuccess => aux._2
            case _                                       => ParserFailure("no es un caracter alfanumerico")
        }
    }
}

class string(cadena: String) extends Parser{
    def apply(entrada: String): ParserOutput = {
        val tamañoDeCadena: Integer = cadena.length
        val cadenaAParsear: String = entrada.take(tamañoDeCadena)
        if (cadena == "")
            ParserFailure("cadena vacia")
        else if (cadenaAParsear == cadena)
            ParserSuccess[String](Some(cadena), entrada.drop(tamañoDeCadena))
        else
            ParserFailure("cadena incorrecta")
    }
}