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
  end
end

class Object
  def chequear_invariantes
    if !invariantes_ok? # verifico que se cumplan todas las invariantes
      raise InvarianteSinCumplir # en caso de que no se cumplan tiro una excepcion
    end
  end

  def invariantes_ok? # verifica que se cumplan todas las invariantes
    self.class.instance_variable_get(:@invariantes).all? {|invariante| instance_eval &invariante}
  end
end