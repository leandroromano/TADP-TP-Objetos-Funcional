class Object
  def self.invariante(&invariante)
    @invariantes ||= [] # inicializo por default como "[]"
    @invariantes.push(invariante) # agrego proc "condicion"
  end

  def chequear_invariantes
    if !invariantes_ok? # verifico que se cumplan todas las invariantes
      raise "No se cumplen todas las invariantes!!!" # en caso de que no se cumplan tiro una excepcion
    end
  end

  def invariantes_ok? # verifica que se cumplan todas las invariantes
    self.class.instance_variable_get(:@invariantes).all? {|invariante| instance_eval &invariante}
  end
end