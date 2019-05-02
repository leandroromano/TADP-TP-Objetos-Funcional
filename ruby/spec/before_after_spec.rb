require_relative "../TP/before_and_after"

class BeforeFlag < RuntimeError
end

class AfterFlag < RuntimeError
end

describe "before_and_after_each_call" do
  context "Sin herencia" do
    class Ejemplo
      attr_accessor :el_bloque_before_se_llamo, :el_bloque_after_se_llamo, :atributo

      before_and_after_each_call( Proc.new{self.el_bloque_before_se_llamo = true}, Proc.new{self.el_bloque_after_se_llamo = true})

      def initialize
        self.el_bloque_before_se_llamo = false
        self.el_bloque_after_se_llamo = false
        self.atributo = 0
      end

      def m
        @atributo += 1
      end
    end

    before(:each) do
      @un_objeto = Ejemplo.new
    end

    it "Debería ejecutar el proc 'before' cuando se llama a un metodo" do
      @un_objeto.m
      expect(@un_objeto.el_bloque_before_se_llamo).to be true
    end

    it "Debería ejecutar el proc 'after' cuando se llama a un metodo" do
      @un_objeto.m
      expect(@un_objeto.el_bloque_after_se_llamo).to be true
    end

    it "Debería retornar el valor original del metodo" do
      expected_return_value = @un_objeto.atributo + 1
      expect(@un_objeto.m).to be expected_return_value
    end

    it "Deberia ejecutarse el proc 'before' antes que el 'after'" do
      class Ejemplo2
        before_and_after_each_call(proc{raise BeforeFlag}, proc{raise AfterFlag})
        def m
        end
      end
      @un_objeto = Ejemplo2.new
      expect{@un_objeto.m}.to raise_exception BeforeFlag
    end

    it "Deberian poder definirse mas de un before_and_after y funcionar correctamente" do
      class Ejemplo3
        attr_accessor :el_bloque_before_se_llamo, :el_bloque_after_se_llamo, :atributo, :counter

        before_and_after_each_call( Proc.new{self.el_bloque_before_se_llamo = true}, Proc.new{self.el_bloque_after_se_llamo = true})
        before_and_after_each_call( Proc.new{self.counter += 1}, Proc.new{})

        def initialize
          self.el_bloque_before_se_llamo = false
          self.el_bloque_after_se_llamo = false
          self.atributo = 0
          self.counter = 0
        end

        def m
          @atributo += 1
        end
      end

      @un_objeto = Ejemplo3.new
      expected_return_value = @un_objeto.instance_variable_get(:@atributo) + 1
      expect(@un_objeto.m).to be expected_return_value
      expect(@un_objeto.el_bloque_before_se_llamo).to be true
      expect(@un_objeto.el_bloque_after_se_llamo).to be true
      expect(@un_objeto.counter).to be 1
    end
  end

  context "Con herencia" do
    class SuperClassEjemplo
      attr_accessor :el_bloque_before_se_llamo, :el_bloque_after_se_llamo, :atributo

      before_and_after_each_call( Proc.new{self.el_bloque_before_se_llamo = true}, Proc.new{self.el_bloque_after_se_llamo = true})

      def initialize
        self.el_bloque_before_se_llamo = false
        self.el_bloque_after_se_llamo = false
        self.atributo = 0
      end

      def m
        @atributo += 1
      end
    end

    it "Una clase que herede de otra que use before_and_after deberia modificar los nuevos metodos que se definan en esta" do
      class Ejemplo4 < SuperClassEjemplo
        def a
          self.atributo
        end
      end
      @un_objeto = Ejemplo4.new

      expect(@un_objeto.a).to be 0
      expect(@un_objeto.el_bloque_before_se_llamo).to be true
      expect(@un_objeto.el_bloque_after_se_llamo).to be true
    end
  end
end