import scala.util.Try
import scala.List

class ParseErrorException(message: String) extends RuntimeException(message)

trait ParserOutput[T] {
    def isParserSuccess: Boolean
    def getSobra: String
    def getResultado: T
}

case class ParserSuccess[T](resultado: T, sobra: String) extends ParserOutput[T] {
    def getResultado: T = resultado
    def getSobra: String = sobra
    def isParserSuccess: Boolean = true
}

case class ParserFailure[T](message: String) extends ParserOutput[T] {
    def getResultado = throw new ParseErrorException(message)
    def getSobra =  throw new ParseErrorException(message)
    def isParserSuccess: Boolean = false
}

sealed trait Parser[T] extends ((String) => ParserOutput[T]) {

    def apply(entrada: String): ParserOutput[T]

    // ---- Combinators ----

    def <|>(parser: Parser[T]): Parser[T] = {
        return  new GenericParser[T](((entrada: String) => {
            val aux: (ParserOutput[T], ParserOutput[T]) = (this(entrada), parser(entrada))
            aux match{
                case (output1, _) if output1.isParserSuccess => output1
                case (_, output2) if output2.isParserSuccess => output2
                case _                                       => ParserFailure[T]("todos fallan")  
            }
        }))
    }

    def <>[A](parser: Parser[A]): Parser[(T, A)] = { 
        return new GenericParser[(T, A)](((entrada: String) => {
            val first: ParserOutput[T] = this(entrada)
            if(!first.isParserSuccess)
                ParserFailure[(T, A)]("el primero fallo")
            else {
                val retornos: (ParserOutput[T], ParserOutput[A]) = (first, parser(first.getSobra))
                
                retornos match {
                    case (fst, snd) if (snd.isParserSuccess) =>
                        ParserSuccess[(T, A)]((fst.getResultado, snd.getResultado), snd.getSobra)
                    case _                                   => 
                        ParserFailure[(T, A)]("el segundo fallo")
                }
            }
        }))
    }

    def ~>[A](parser: Parser[A]): Parser[A] = {
        return new GenericParser[A](((entrada: String) => {
            var first: ParserOutput[T] = this(entrada)
        
            first match {
                case ParserFailure(message) => ParserFailure[A](message)
                case r                      => parser(r.getSobra)
            }

        }))
    }

    def <~[A](parser: Parser[A]): Parser[T] = {
        return new GenericParser[T](((entrada: String) => {
            var first: ParserOutput[T] = this(entrada)
            if (!first.isParserSuccess)
                ParserFailure[T]("el primero fallo")
            else {
                val second: ParserOutput[A] = parser(first.getSobra)

                second match {
                    case ParserFailure(message)  => ParserFailure[T]("el segundo fallo: " + message)
                    case ParserSuccess(_, sobra) => ParserSuccess[T](first.getResultado, sobra)
                }
            }
        }))
    }
 // TODO: Devuelve una lista de elementos del 1er parser.
    def sepBy[A](parser: Parser[A]): Parser[List[T]] = {
        return new GenericParser[List[T]](((entrada: String) => {
            var retorno: List[T] = List()
            val parserCompuesto = (parser ~> this)+ 
            var parsed: ParserOutput[List[T]] = this.map(r => List(r))(entrada)

            parsed match {
                case ParserSuccess(rs, sobra) if parserCompuesto(sobra).isParserSuccess =>
                    ParserSuccess[List[T]](rs ::: parserCompuesto(sobra).getResultado, parserCompuesto(sobra).getSobra)
                case ParserFailure(message)                                             =>
                    ParserFailure[List[T]](message)
                case soloParseaUnSoloElemento                                           =>
                    soloParseaUnSoloElemento
            }
        }))
    }


    // ---- Operaciones ----

    type Condicion = String => Boolean
    def satisfies(condicion: Condicion): Parser[T] = {
        return new GenericParser[T](((entrada: String) => {
            var retorno: ParserOutput[T] = this(entrada)
            retorno match {
                case _ if !condicion(entrada) => ParserFailure[T]("no se cumple condicion de entrada")
                case r                        => r
            }
        }))
    }

    def opt: Parser[Option[T]] = {
        return new GenericParser[Option[T]](((entrada: String) => {
            var retorno: ParserOutput[T] = this(entrada)

            retorno match {
                case ParserSuccess(value, sobra) => ParserSuccess[Option[T]](Some(value), sobra)
                case _                           => ParserSuccess[Option[T]](None, entrada)
            }
        }))
    }

    def *(): Parser[List[T]] = {
        return new GenericParser[List[T]](((entrada: String) => {
            var retorno: List[ParserOutput[T]] = List()
            var output: ParserOutput[T] = this(entrada)

            if (!output.isParserSuccess)
                ParserSuccess[List[T]](List(), entrada)
            else {
                while(output.isParserSuccess){
                    retorno = retorno ::: List[ParserOutput[T]](output)
                    output = this(output.getSobra)
                }
                ParserSuccess[List[T]](retorno.map(_.getResultado), retorno.last.getSobra)
            }    
        }))
    }

    def +(): Parser[List[T]] =  {
        return new GenericParser[List[T]](((entrada: String) => {
            var retorno: List[ParserOutput[T]] = List()
            var output: ParserOutput[T] = this(entrada)

            if (!output.isParserSuccess)
                ParserFailure[List[T]]("ERROR") // Modificar
            else {
                while(output.isParserSuccess){
                    retorno = retorno ::: List[ParserOutput[T]](output)
                    output = this(output.getSobra)
                }
                ParserSuccess[List[T]](retorno.map(_.getResultado), retorno.last.getSobra)
            }
        }))
    }

    def const[A](parametro: A): Parser[A] = {  // Creo que es asi (?
        return new GenericParser[A](((entrada: String) => {
            val old: ParserOutput[T] = this(entrada)

            old match {
                case ParserSuccess(_, sobra) => ParserSuccess[A](parametro, sobra)
                case ParserFailure(message)  => ParserFailure[A](message)
            }
        }))
    }
    
    def map[A](transformacion: T => A): Parser[A] = {
        return new GenericParser[A](((entrada: String) => {
            val old: ParserOutput[T] = this(entrada)

            old match {
                case ParserSuccess(retorno, sobra) => ParserSuccess[A](transformacion(retorno), sobra)
                case ParserFailure(message) => ParserFailure[A](message)
            }
        }))
    }
}

class GenericParser[T](comportamiento: (String) => ParserOutput[T]) extends Parser[T]{
    def apply(entrada: String): ParserOutput[T] = {
        this.comportamiento(entrada)
    }
}

class anyChar() extends Parser[Char]{
    def apply(entrada: String): ParserOutput[Char] = {
        entrada.toList match {
            case List() => ParserFailure[Char]("texto vacio")
            case c :: s => ParserSuccess[Char](c, s.mkString)
        }
    }
}

class char(caracter: Char) extends Parser[Char]{
     def apply(entrada: String): ParserOutput[Char] = {
        entrada.toList match {
            case List()                  => ParserFailure[Char]("caracter no encontrado")
            case c :: s if c == caracter => ParserSuccess[Char](c, s.mkString)
            case c :: s                  => ParserFailure[Char]("caracter incorrecto")
        }
    }
}

class void() extends Parser[Unit] {
     def apply(entrada: String): ParserOutput[Unit] = {
        entrada match {
            case "" => ParserFailure[Unit]("texto vacio")
            case _  => ParserSuccess[Unit]((), entrada.tail)
        }
    }
}

class letter() extends Parser[Char]{
     def apply(entrada: String): ParserOutput[Char] = {
        entrada.toList match {
            case List()               => ParserFailure[Char]("texto vacio")
            case c :: s if c.isLetter => ParserSuccess[Char](c, s.mkString)
            case _                    => ParserFailure[Char]("no es una letra")
        }
    }
}

class digit() extends Parser[Char]{
    def apply(entrada: String): ParserOutput[Char] = {
        entrada.toList match {
            case List()              => ParserFailure[Char]("texto vacio")
            case c :: s if c.isDigit => ParserSuccess[Char](c, s.mkString)
            case _                   => ParserFailure[Char]("no es un digito")
        }
    }
}

class alphaNum() extends Parser[Char]{
    def apply(entrada: String): ParserOutput[Char] = { 
        var aux: (ParserOutput[Char], ParserOutput[Char]) = (new letter()(entrada), new digit()(entrada))
        aux match {
            case (output1, _) if output1.isParserSuccess => aux._1
            case (_, output2) if output2.isParserSuccess => aux._2
            case _                                       => ParserFailure[Char]("no es un caracter alfanumerico")
        }
    }
}

class string(cadena: String) extends Parser[String]{
    def apply(entrada: String): ParserOutput[String] = {
        val tamañoDeCadena: Integer = cadena.length
        val cadenaAParsear: String = entrada.take(tamañoDeCadena)
        if (cadena == "")
            ParserFailure[String]("cadena vacia")
        else if (cadenaAParsear == cadena)
            ParserSuccess[String](cadena, entrada.drop(tamañoDeCadena))
        else
            ParserFailure[String]("cadena incorrecta")
    }
}