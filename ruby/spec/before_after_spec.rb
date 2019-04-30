require "C:/Users/juanb/Desktop/MisProyectos/Ruby/grupo9-2019-1c/ruby/TP/before_and_after.rb"

class Ejemplo
  before_and_after_each_call(proc {self.el_bloque_before_se_llamo = true}, proc {self.el_bloque_after_se_llamo = true})
  attr_accessor :el_bloque_before_se_llamo,
                :el_bloque_after_se_llamo
  def initialize
    self.el_bloque_before_se_llamo = false
    self.el_bloque_after_se_llamo = false
    @atributo = 0
  end

  def m
    @atributo += 1
  end
end

describe "before_and_after_each_call" do
  before(:each) do
    @un_objeto = Ejemplo.new
  end

  it "Debería ejecutar el proc 'before' cuando se llama a un metodo" do
    @un_objeto.m
    expect(@un_objeto.el_bloque_before_se_llamo).to be true
  end

  it "Debería ejecutar el proc 'after' cuando se llama a un metodo" do
    @un_objeto.m
    expect(@un_objeto.el_bloque_before_se_llamo).to be true
  end

end