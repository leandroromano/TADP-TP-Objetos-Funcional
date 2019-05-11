require_relative "pre_post"
require_relative "invariantes"

class Module
  def before_and_after_each_call(before, after)
    ensure_initialized_overriden_methods_befores_and_afters
    @befores.push(before) # agrego proc "before"
    @afters.push(after) # agrego proc "after"
    define_singleton_method :method_added do |method| # modifico el metodo de clase "method_added"
      redefinir_metodo method # redefino el metodo con las caracteristicas pedidas
    end
  end

  def ensure_initialized_overriden_methods_befores_and_afters # Me aseguro que esten inicializados como listas  para que no sean nil
    @befores ||= [] # inicializo por default
    @afters ||= [] # inicializo por default
    @overriden_methods = [] # inicializo. Incluyo "initialize": algunas invariantes involucran atributos que se inicializan en este metodo. Evito que initialize se redefina para que no ocurra un error al tratar con atributos dentro de invariantes no inicializados
  end

  def overriden_method? method
    @overriden_methods.include? method
  end

  def redefinir_metodo method
    if !overriden_method? method # control para que haya un bucle de definicion y redefinicion infinito
      @overriden_methods.push method # agrego metodo a la lista de metodos sobreescritos
      aux = self.instance_method(method) #unbound. Guardo el metodo original
      if method == :initialize   # hago una excepcion para "initialize", asi se le puden poner pres y posts.
        set_pres_and_posts method
        define_method method do |*args|
          retorno = aux.bind(self).call(*args)
          self.check_post method, retorno
          self.chequear_invariantes
          retorno
        end
      else
        befores = @befores
        afters = @afters
        set_pres_and_posts method
        define_method method do |*args| # redefino el metodo original con el mismo nombre y sus argumentos
          befores.reverse_each{|p| instance_exec method, &p} # evaluo todos los procs "before" dentro del contexto de la instancia correspondiente
          retorno = aux.bind(self).call(*args) # bindeo y ejecuto el metodo original. Guardo el valor de retorno
          afters.each{|p| instance_exec method, retorno, &p} # evaluo todos los procs "after" dentro del contexto de la instancia correspondiente
          retorno # retorno original
        end
      end
    end
  end
end
