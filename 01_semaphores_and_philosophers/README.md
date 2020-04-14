# STM 

## Introduccion

Bien ahora que sabemos como modelar una variable que es mutable y que necesitamos funciones para escribir y leer esta variable, para controlar la concurrencia entre threads de este estado habra que hacerlo por medio de un mecanismo de control que es STM. 

En el modelo tradicional de programación con threads, cuando compartimos variables o estado entre distintos threads, mantenemos la consistencia del estado utilizando locks, y notificamos a los threads de los cambios usando variables condicionales. Las variables mutables de Haskell implementan una versión mejorada de este esquema, pero sufre de los mismos problemas que en otros lenguajes

- Race conditions a los locks olvidados de liberar
- Deadlocks que resulten de locks inconsistentes
- Corrupción si hay excepciones no capturadas
- Notificaciones omitidas puede traducirse en pérdida de notificación al thread.

Estos problemas afectan a cualquier escala de software usando este esquema, por lo que es bastante complejo el manejo de threads con estado compartido, en nuestro caso con `IORef`s

Pero que es STM, ademas de memoria transaccional? Es un mecanismo de lock optimista.

Software transactional memory (STM) nos da unas herramientas básicas aunque potentes con las cuales podemos solucionar casi todos los problemas ya mencionados. STM ejecuta un bloque de acciones como una transacción usando un combinador llamado atomically, en suma este combinador nos permite convertir una transacción STM en un bloque ejecutable. Una vez que entramos al bloque, otros threads no pueden ver ninguna modificaciones que hagamos hasta que salgamos, y nuestro thread no puede ver ninguno de los cambios hechos por otros threads. Estas dos propiedades hacen que nuestra ejecución sea aislada. Esto nos hace pensar a algo muy similar a un mutex, en el que no se permite a otro thread modificar un estado hasta que lo libere.

![STM y mutex](https://raw.githubusercontent.com/arquitecturas-concurrentes/iasc-stm-haskell-2019/master/01_semaphores_and_philosophers/mutex.png)

Cuando se sale de una transacción, solo una de las dos siguientes cosas pueden suceder:

- Si no hay otro thread que haya modificado concurrentemente el mismo estado que nosotros, todas nuestras modificaciones serán visibles automáticamente a otros threads

- Toas las modificaciones son descartadas sin ser ejecutadas, y nuestro bloque de acciones es reiniciado automáticamente.

La naturaleza de ejecutar todo o nada del bloque atomically se ejecuta de manera atómica, por eso se llama asi :P .
Esto es una de las propiedades ACID que vemos en las bases de datos, y es por eso que trabajando con STM se ve que es algo medianamente similar aunque más simple.

Veamos un ejemplo simple para entender esta parte teorica, y que mejor que nuestro viejo ejemplo del contador...

vamos a ver que nuestro contador en vez de ser un `IORef` va a ser una abstraccion que pueda usar STM en funcion de la interfaz que tiene y que esta en parte basado en `IORef`, el mismo se llama `TVar`, vamos a crear un type alias que es similar al ejemplo anterior pero en funcion de `TVar`

```haskell
type Counter = TVar Int
```

Bien ahora veamos de crear nuestro sucesor, la firma que tendra es la siguiente

```haskell
incTVar :: Counter -> STM ()
```

Interesante ahora en vez de pasar `IO ()` devolvemos `STM ()`, y esto que es?. Es una accion tambien como lo es IO pero `STM a` sera una accion que es mucho mas cerrada, en cuanto a las operaciones que admite, si bien podemos siempre modelar con un tipo mas abarcativo como lo es IO, no es necesario ya que queremos que nuestro contador se maneje en el mundo de `STM`, por lo que dentro de este tipo de acciones se va a poder escribir y leer variables transaccionales, y estas son siempre del tipo `TVar a`, de esta manera vamos a tener siempre las funciones de escritura y lectura, la firma de las mismas es:

```haskell
readTVar  :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()
```

buenismo, ahora entendemos la firma, y los metodos que usamos para leer y escribir una variable mutable y transaccional, escribamos el contador.

```haskell
incTVar counter = do
    val <- readTVar counter
    writeTVar counter (val + 1)
```

De la parte mas teorica STM tiene a los `TVar` que contienen un estado compartido, cuando un thread ejecuta una transacción, crea un log para el solo, en STM tenemos las dos operaciones que son `writeTVar` y `readTVar`, cuando se modifica una `TVar` por medio de `writeTVar`, no se pisa este valor pero se lo registra en el log, o sea el log es como un diario que solo usaremos dentro del contexto de la transacción para un thread en particular, y se desecha al salir del bloque de la transacción.

Hablamos de un contexto en el cual un recurso como un `TVar` va a estar bloqueado por un solo thread hasta que no salga de ese mismo contexto. Hasta ahora no lo vimos, y es algo que ya nos da `STM` y que se llama `atomically`

Su firma es 

```haskell
atomically :: STM a -> IO a
```

Interesante, toma una accion de STM y nos va a dar una accion de IO que es mas libre. lo que siempre tenemos en `atomically acc` podemos meterlo en una do-notation y de esta manera podemos ejecutar lo que querriamos que se maneja atomicamente dentro de ese `contexto` atomico. Entonces en STM en haskell, las `TVar`s nos proveen con la abstraccion que tendria el estado o variable mutable y generaria el log de las operaciones y `atomically` nos daria un contexto que querriamos que se use como si fuese una operacion. Esto es para que una transaccion se pueda rechazar o aceptar una vez que se termna lo que se hizo en ese bloque. 

Que nos da `atomically` para terminar con esta parte?

- Atomicidad: Los efectos despues de salir de un bloque de atomically, lo van a poder ver los otros threads pero no mientras haya un lock, esto garantiza que si hay mas de una operacion dentro de este bloque que sea de lectura y escritura, vamos a estar seguros que no fue modificado entre operaciones por otro thread.
- Aislamiento: De lo que mencionamos en el punto anterior, mientras estemos dentro del bloque de atomically, otros threads no van a poder modificar nuestra variables involucradas, y cualquier operacion que quieran hacer sobre ellas fallara.
  
Bien ahora terminemos el ejemplo del contador.. ahora usemos el contador e incrementemos el valor, y antes de eso imprimiendo el valor

```haskell
exec :: Counter -> IO()
exec cont = do
    hPutStr stdout "Incrementando el contador en 1..."
    incTVar cont

main = do
    cont <- atomically (newTVar 3)
    exec cont 
```
Lo que sucedera es que esto nos va a fallar... y porque? porque si bien incTVar es una operacion de STM, hPutStr es de IO, y por lo tanto al ser acciones de distinto tipo, no pueden usarse dentro de una do-notation

Y que podriamos hacer entonces???
Bueno veamos que el bloque atomically si genera una accion de IO () como retorno entonces podriamos tan solo cambiar el exec agregando un atomically a la operacion de usar la funcion incTVar con el contador.

```haskell
exec :: Counter -> IO()
exec cont = do
    hPutStr stdout "Incrementando el contador en 1..."
    atomically (incTVar cont)

main = do
    cont <- atomically (newTVar 3)
    exec cont 
```

### Usando un poco mas STM

Ahora veamos un caso un poco mas complicado, el caso en el que pueda generarse un efecto que no se esperaba o cuando esta el camino en el que detectemos un fallo bajo una condicion. Pero antes de eso se pueden generar otros escenarios, cuando hablamos de transaccionalidad, lo vimos en concepto del bloque ``atomically`, dentro de ese bloque los TVars que vayamos usando crearan un log y una vez que se termine ese log se tiene que chequear si todos los pasos fueron realizados exitosamente, ahi y solo ahi se commiteara la transaccion y el valor quedara impactado finalmente en estas variables mutables, pero que pasa si fallara alguna validacion o alguna operacion? Como STM tiene que garantizar la atomicidad de la operacion si no se puede comittear la operacion ya que puede quedar las variables en un estado inconsistente, entonces debe volver a intentar y eso se conoce en STM como `re-excecution`. Debido a que estos cambios se hacen aun en memoria, tan solo se vuelve al estado inicial y se vuelve a intentar la operacion. Bien, hasta ahi todo muy lindo pero que pasa si hay algo que se sale por afuera de ese control de STM, por ejemplo un efecto de `IO`, y peor aun, esto se hace en un contexto de atomicidad como el siguiente ejemplo?

```haskell
efectoIO :: IO()
efectoIO = hPutStr stdout "Hola efecto de lado!"

main = do
    a <- atomically (newTVar 2)
    b <- atomically (newTVar 1)
    atomically ( do
        x <- readTVar a
        y <- readTVar b
        if y<x then efectoIO
               else return())
```

Bien en ese caso si y es menor a x, se generara un efecto de IO y al ser este efecto algo que puede generar una posible inconsistencia en los datos y que entonces se tenga que volver a re ejercutar todo, pero igualmente esto por suerte va a fallar incluso antes, porque como vimos en el ejemplo del contador, no podemos tener una operacion de IO combinada con una de STM, por lo que deberia fallar en compilacion.

Perfecto, pero entonces como hacemos para volver a reintentar la operacion?? Utilizando la funcion retry, que tiene la siguiente firma:

```haskell
retry :: STM a
```

veamos un ejemplo en el que queremos hacer esto, que es el de semaforos:

```haskell
type Semaphore = TVar Bool

newSem :: Bool -> IO Semaphore
newSem aval = newTVarIO aval

p :: Semaphore -> STM()
p sem = do
    b <- readTVar sem
    if b
        then writeTVar sem False
        else retry

v :: Semaphore -> STM()
v sem = writeTVar sem True
```

cada vez que nosotros queremos chequear con el flag si se puede entrar o no en la condicion, vamos a tener que ver si el flag esta seteado en false, que significa que el recurso que maneja el semaforo esta siendo usado. Si este valor esta en false, deberia abortarse la operacon y solo cuando se termine se seteara en True, si bien despues podemos agregar el atomically, esto siempre es en funcion de como se usa.

Podemos incluso cambiar esto como una funcion check que si dado una condicion nos da falso, deberiamos abortar la operacion, extrayendo esto en una funcion podemos dejar esto como

```haskell
check :: Bool -> STM ()
check True  = return ()
check False = retry
```

ahora esto lo podemos usar en alguna condicion que en el caso de que falle el booleano nos haga la re ejecucion del bloque

```haskell
type Resource = TVar Int

check :: Bool -> STM ()
check True  = return ()
check False = retry

acquire :: Resource - Int - STM ()
acquire res nr = do n <- readTVar res
                    check ( nr >= n )
                    writeTVar res (n - nr)
                            
release :: Resource - Int - STM ()
release res nr = do n <- readTVar res
                    writeTVar res (n + nr)
```

El ultimo caso es el del `orElse`, donde es un caso en el cual si uno de los valores es nulo o no puede cumplirse, optara por el camino que puede tomar como segundo parametro, la firma es la siguiente:

```haskell
orElse :: STM a -> STM a -> STM a
```

Como funcionaria esta funcion? Bueno es como un combinador, tiene dos acciones de STM, si la primera falla, o sea recibe un retry, no sigue intentando y en su lugar opta por la segunda accion y retorna esta si no falla.

Veamos un ejemplo bien simple de esto.

```haskell
someFailingOp :: TVar a -> STM ()
someFailingOp a =
    v <- readTVar a
    if v < 49494949
        then retry
        else return ()

successOp :: TVar a -> STM()
successOp a =
    v <- readTVar a
    if v > 0
        then writeTVar a 33723
        else retry

anElseOperation :: TVar a -> TVar a -> STM () 
anElseOperation a b =
    OrElse (someFailingOp a) (successOp b)

main = do
    a <- atomically (newTVar 2)
    b <- atomically (newTVar 1)
    atomically (anElseOperation a b)
```

#### Ejercicio de los filosofos que cenan

![filosofos](https://raw.githubusercontent.com/arquitecturas-concurrentes/iasc-stm-haskell-2019/master/01_semaphores_and_philosophers/filosofos.png)

Explicamos TMVar, que es?


