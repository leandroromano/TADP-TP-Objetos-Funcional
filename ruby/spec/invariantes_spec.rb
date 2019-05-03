require_relative "../TP/invariantes"

describe "invariantes" do
  class Ejemplo
    attr_accessor :atributo
    invariante {atributo > 0}
    def initialize
      atributo = -1
    end

    def m
      "Hola"
    end
  end
  it "Deberia chequear invariantes cuando llama a un metodo" do
    @un_objeto = Ejemplo.new
    expect{@un_objeto.m}.to raise_exception InvarianteSinCumplir
  end
end