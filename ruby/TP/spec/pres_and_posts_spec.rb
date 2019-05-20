require 'rspec'
require_relative "../main/invariantes"

describe 'pres_and_posts' do
  class Ejemplo
    attr_accessor :atributo
    def initialize valor
      self.atributo = valor
    end
    post {self.atributo > 0}
    def m
      self.atributo -= 1
    end

    pre {self.atributo != 0}
    def a
      self.atributo
    end
  end

  it "Deberia lanzar una excepcion luego de utilizar un metodo cuando su postcondicion no se cumple" do
    un_objeto = Ejemplo.new(0)
    expect {un_objeto.m}.to raise_exception NoSeCumplePostcondicion
  end

  it "No deberia lanzar una excepcion luego de utilizar un metodo cuando su postcondicion se cumple" do
    un_objeto = Ejemplo.new(2)
    expect{un_objeto.m}.not_to raise_exception
  end

  it "Deberia lanzar una excepcion luego de utilizar un metodo cuando su precondicion no se cumple" do
    un_objeto = Ejemplo.new(0)
    expect{un_objeto.a}.to raise_exception NoSeCumplePrecondicion
  end

  it "No deberia lanzar una excepcion luego de utilizar un metodo cuando su precondicion se cumple" do
    un_objeto = Ejemplo.new 10
    expect{un_objeto.a}.not_to raise_exception
  end

  context "Para el metodo 'initialize'" do
    class Ejemplo2
      attr_accessor :atributo

      post {self.atributo != 0}
      def initialize value
        self.atributo = value
      end
    end

    it "Deberia lanzar una excepcion cuando no se cumple la postcondicion del metodo 'initialize'" do
      expect{Ejemplo2.new 0}.to raise_exception NoSeCumplePostcondicion
    end

    it "No deberia lanzar una excepcion cuando se cumple la postcondicion del metodo 'initialize'" do
      expect{Ejemplo2.new 2}.not_to raise_exception
    end
  end

  context 'Permite el uso de parametros especificos del metodo' do
    class Ejemplo3
      pre {divisor != 0}
      post {|resultado| resultado * divisor == dividendo}
      def dividir(dividendo, divisor)
        dividendo / divisor
      end
    end

    it "No deberia lanzar una excepcion cuando los parametros del metodo, cumplen la postcondicion" do
      un_objeto = Ejemplo3.new
      expect {un_objeto.dividir(4,2)}.not_to raise_exception
    end

    it "Deberia lanzar una excepcion cuando uno de los parametros del metodo, no cumple la precondicion" do
      un_objeto = Ejemplo3.new
      expect {un_objeto.dividir(4, 0)}.to raise_exception NoSeCumplePrecondicion
    end
  end
end