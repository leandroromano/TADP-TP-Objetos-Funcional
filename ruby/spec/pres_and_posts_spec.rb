require 'rspec'
require_relative "../TP/invariantes"

describe 'pres_and_posts' do
  class Ejemplo
    attr_accessor :atributo
    before_and_after_each_call(proc{},proc{})
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
end