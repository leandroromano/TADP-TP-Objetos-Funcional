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
    @overriden_methods = []
  end

  def overriden_method? method
    @overriden_methods.include? method
  end

  def redefinir_metodo method
    if !overriden_method? method # control para que haya un bucle de definicion y redefinicion infinito
      @overriden_methods.push method # agrego metodo a la lista de metodos sobreescritos
      aux = self.instance_method(method) #unbound. Guardo el metodo original
      params = self.instance_method(method).parameters.map{|list| list[1]}
      if method == :initialize   # hago una excepcion para "initialize", asi se le puden poner pres y posts.
        set_pres_and_posts method
        define_method method do |*args|
          method_params = params.zip(args).to_h
          context = Context.new method_params, self
          retorno = aux.bind(self).call(*args)
          context.check_post method, retorno
          context.chequear_invariantes
          retorno
        end
      else
        befores = @befores
        afters = @afters
        set_pres_and_posts method
        define_method method do |*args| # redefino el metodo original con el mismo nombre y sus argumentos
          method_params = params.zip(args).to_h
          context = Context.new method_params, self
          befores.reverse_each{|p| context.instance_exec method, &p} # evaluo todos los procs "before" dentro del contexto de la instancia correspondiente
          retorno = aux.bind(self).call(*args) # bindeo y ejecuto el metodo original. Guardo el valor de retorno
          afters.each{|p| context.instance_exec method, retorno, &p} # evaluo todos los procs "after" dentro del contexto de la instancia correspondiente
          retorno # retorno original
        end
      end
    end
  end
end
