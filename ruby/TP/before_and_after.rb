class Module
  def before_and_after_each_call(before, after)
    ensure_initialized_overriden_methods_befores_and_afters
    @befores.push(before) # agrego proc "before"
    @afters.push(after) # agrego proc "after"
    define_singleton_method :method_added do |method| # modifico el metodo de clase "method_added"
      if @overriden_methods.nil?    # solo va a pasar cuando se trate de una subclase de la clase que definio el before_and_after
        @overriden_methods = [:initialize] # al no contener ningun otro metodo, permite que la subclase pueda sobrescribir_metodos de la superclase y estos sean modificados por el before_and_after como es debido
        @befores = superclass.instance_variable_get :@befores # para que la subclase conosca los befores, definidos por la superclase
        @afters = superclass.instance_variable_get :@afters # para que la subclase conosca los afters, definidos por la superclase
      end
      if !overriden_method? method # control para que haya un bucle de definicion y redefinicion infinito
        @overriden_methods.push method # agrego metodo a la lista de metodos sobreescritos
        redefinir_metodo method # redefino el metodo con las caracteristicas pedidas
      end
    end
  end

  def ensure_initialized_overriden_methods_befores_and_afters # Me aseguro que esten inicializados como listas  para que no sean nil
    @overriden_methods = [:initialize] # inicializo. Incluyo "initialize": algunas invariantes involucran atributos que se inicializan en este metodo. Evito que initialize se redefina para que no ocurra un error al tratar con atributos dentro de invariantes no inicializados
    @befores ||= [] # inicializo por default como "[]"
    @afters ||= [] # inicializo por default como "[]"
  end

  def overriden_method? method
    @overriden_methods.include? method
  end

  def redefinir_metodo method
    aux = self.instance_method(method) #unbound. Guardo el metodo original
    befores = @befores
    afters = @afters
    define_method method do |*args| # redefino el metodo original con el mismo nombre y sus argumentos
      befores.reverse_each{|p| instance_eval &p} # evaluo todos los procs "before" dentro del contexto de la instancia correspondiente
      retorno = aux.bind(self).call(*args) # bindeo y ejecuto el metodo original. Guardo el valor de retorno
      afters.each{|p| instance_eval &p} # evaluo todos los procs "after" dentro del contexto de la instancia correspondiente
      retorno # retorno original
    end
  end
end