module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Material = Material {
    nombre :: String,
    calidad :: Number
} deriving (Show, Eq)

data Edificio = Edificio {
    tipoEdificio :: String,
    materiales :: [Material]
} deriving (Show, Eq)

data Aldea = Aldea {
    poblacion :: Number,
    materialesDisponibles :: [Material],
    edificios :: [Edificio]
} deriving (Show, Eq)

-- Punto 1
-- a. esValioso 
-- recibe un material y retorna true si su calidad es mayor a 20
esValioso :: Material -> Bool
esValioso = (>20) . calidad

-- b. unidadesDisponibles 
-- recibe el nombre de un material y una aldea y retorna la cantidad de materiales disponibles con ese nombre en la aldea
--  > disponibles "Acero" (Aldea 50 [(Material "Acero" 15), (Material "Acero" 20), (Material "Piedra" 5)] [])
--    2
unidadesDisponibles :: String -> Aldea -> Number
unidadesDisponibles nombreMaterial = length . filter ((==nombreMaterial) . nombre) . materialesDisponibles

-- c. valorTotal 
-- recibe una aldea y retorna la suma de la calidad de todos los materiales que hay en la aldea. Estos son tanto los disponibles como los usados en sus edificios.
-- > valorTotal (Aldea 50 [(Material "Acero" 15), (Material "Piedra" 5)] [(Edificio "Barracas" [(Material "Acero" 20)])])
--    40
valorTotal :: Aldea -> Number
valorTotal aldea = sum $ map calidad $ materialesDisponibles aldea ++ concatMap materiales (edificios aldea)


aldea1 :: Aldea
aldea1 = Aldea {
    poblacion = 50,
    materialesDisponibles = [Material "Acero" 15, Material "Piedra" 5, Material "Madera" 10],
    edificios = [Edificio "Barracas" [Material "Acero" 20], Edificio "Casa" [Material "Acero" 15]]
}

aldea2 :: Aldea
aldea2 = Aldea {
    poblacion = 100,
    materialesDisponibles = [Material "Acero" 25, Material "Piedra" 10, Material "Madera" 20],
    edificios = [Edificio "Barracas" [Material "Acero" 50], Edificio "Casa" [Material "Madera" 15]]
}

-- Desarrollar las siguientes tareas para que los gnomos puedan realizar en una aldea: 
type Tarea = Aldea -> Aldea

-- a. tenerGnomito aumenta la población de la aldea en 1. 
tenerGnomito :: Tarea
tenerGnomito aldea = aldea {poblacion = (+1) . poblacion $ aldea}

-- b. lustrarMaderas 
-- aumenta en 5 la calidad de todos los materiales disponibles cuyo nombre empiece con la palabra "Madera". 
-- El resto de los materiales disponibles de la aldea no deberían verse afectados al realizar esta tarea. 
aumentarCalidad :: Number -> Material -> Material
aumentarCalidad cantidad material = material {calidad = calidad material + cantidad}

filtrarMaderas :: Aldea -> [Material]
filtrarMaderas = filter ((=="Madera") . nombre) . materialesDisponibles

lustrarMaderas :: Tarea
lustrarMaderas aldea = aldea {materialesDisponibles = map (aumentarCalidad 5) (filtrarMaderas aldea) ++
                                                    filter ((/="Madera") . nombre) (materialesDisponibles aldea)}

-- c. 
-- recolectar que dado un material y una cantidad de cuánto de ese material se quiere recolectar, 
-- incorpore a los materiales disponibles de la aldea ese mismo material tantas veces como se indique. No usar where
recolectar :: Material -> Number -> Tarea
recolectar material cantidad aldea = aldea { materialesDisponibles = materialesDisponibles aldea ++ replicate cantidad material }

-- Realizar las consultas que permitan:
-- a. Obtener los edificios chetos de una aldea, que son aquellos que tienen algún material valioso.
edificiosChetos :: Aldea -> [Edificio]
edificiosChetos = filter (any esValioso . materiales) . edificios

-- b. Obtener una lista de nombres de materiales comunes, que son aquellos que se encuentran en todos los edificios de la aldea.
edificioConMismoMaterial :: Aldea -> Material
edificioConMismoMaterial aldea = head $ materiales $ head $ edificios aldea

materialesComunes :: Aldea -> Material
materialesComunes aldea = head $ concatMap (filter (==edificioConMismoMaterial aldea) . materiales) (edificios aldea)

-- 4.
-- a. realizarLasQueCumplan 
-- recibe una lista de tareas, un criterio que debería cumplir la aldea luego de realizar cada tarea y la aldea inicial, 
-- y retorne cómo quedaría la aldea si se realizaran las tareas válidas, una tras otra. Una tarea es válida si, 
-- después de realizarse sobre una aldea (la original o la resultante de haber realizado otra tarea previa), la misma cumple con el criterio indicado.
realizarLasQueCumplan :: [Tarea] -> (Aldea -> Bool) -> Tarea
realizarLasQueCumplan [] _ aldea = aldea
realizarLasQueCumplan (tarea1:tarea2:tareas) criterio aldea 
    | (criterio . tarea1) aldea = realizarLasQueCumplan (tarea2:tareas) (criterio . tarea1) aldea
    | otherwise                 = realizarLasQueCumplan (tarea2:tareas) criterio aldea

-- b. Hacer consultas utilizando realizarLasQueCumplan de forma tal que: 
--  i. Utilizando realizarLasQueCumplan. Se tengan gnomitos 3 veces (como 3 tareas independientes entre sí), asegurando que 
--     siempre haya más unidades de comida disponible que la cantidad de población en la aldea luego de realizar cada tarea
--     de tener gnomitos. Un material con el nombre "Comida" se considera una unidad de comida. 
-- ii. Se recolecten 30 unidades de madera de pino de calidad igual a la calidad máxima de las maderas disponibles y 
--     luego se lustren las maderas disponibles de la aldea, asegurando siempre que todos los materiales disponibles sean valiosos.
--     puedes usar realizarLasQueCumplan
tieneMasComidaQuePoblacion :: Aldea -> Bool
tieneMasComidaQuePoblacion aldea = (> poblacion aldea) . unidadesDisponibles "Comida" $ aldea

todosLosMaterialesValiosos :: Aldea -> Bool
todosLosMaterialesValiosos = all esValioso . materialesDisponibles
