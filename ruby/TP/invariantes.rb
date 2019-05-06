require_relative "./before_and_after"

class InvarianteSinCumplir < RuntimeError
  def message
    "No se cumplen todas las invariantes."
  end
end

class Module
  def invariante(&invariante)
    @invariantes ||= [] # inicializo por default como "[]"
    @invariantes.push(invariante) # agrego proc "condicion"
    unless @invariantes_chequeando
      before_and_after_each_call(proc {}, proc {self.chequear_invariantes})
      @invariantes_chequeando = true
    end
  end
end

class Object
  def chequear_invariantes
    if !invariantes_ok? # verifico que se cumplan todas las invariantes
      raise InvarianteSinCumplir # en caso de que no se cumplan tiro una excepcion
    end
  end

  def invariantes_ok? # verifica que se cumplan todas las invariantes
    invariantes = self.class.instance_variable_get(:@invariantes) || []
    invariantes.all? {|invariante| instance_eval &invariante}
  end
end