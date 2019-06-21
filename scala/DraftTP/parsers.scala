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

    def <|>(parser: Parser): Parser = {
        return  new DefaultParser(((entrada: String) => {
            val aux: (ParserOutput, ParserOutput) = (this(entrada), parser(entrada))
            aux match{
                case (output1, _) if output1.isParserSuccess => output1
                case (_, output2) if output2.isParserSuccess => output2
                case _                                       => ParserFailure("todos fallan")  
            }
        }))
    }

    def <>(parser: Parser): Parser = {
        return new DefaultParser(((entrada: String) => {
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
        return new DefaultParser(((entrada: String) => {
            var retorno: ParserOutput = this(entrada)
        
            retorno match {
                case r if !r.isParserSuccess => retorno
                case r                       => parser(r.getSobra)
            }

        }))
    }

    def <~(parser: Parser): Parser = {
        return new DefaultParser(((entrada: String) => {
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
        return new DefaultParser(((entrada: String) => {
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
    
}

class DefaultParser(comportamiento: (String) => ParserOutput) extends Parser{
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