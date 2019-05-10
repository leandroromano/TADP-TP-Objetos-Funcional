require_relative "../main/invariantes"

describe "invariantes" do
  class Ejemplo
    attr_accessor :atributo

    invariante {atributo > 0}
    invariante {atributo < 10}

    def initialize valor
      self.atributo = valor
    end

    def m
      self.atributo -= 2
    end
  end

  it "No deberia lanzar un error cuando las invariantes se cumplen" do
    @un_objeto = Ejemplo.new 9
    expect{@un_objeto.m}.to_not raise_exception InvarianteSinCumplir
  end

  it "Deberia lanzar un error cuando alguna de las invariantes no se cumple" do
    @un_objeto = Ejemplo.new 1
    expect{@un_objeto.m}.to raise_exception InvarianteSinCumplir
  end
end
