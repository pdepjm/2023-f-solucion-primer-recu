module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Actividad = String

data Promesa = UnaPromesa {
	tema:: String,
	esBeneficiario:: Persona -> Bool,
	costo:: Number
} 

data Candidato = UnCandidato {
	nombre:: String,
    partido:: String,
	promesas:: [Promesa]
} 

data Persona = UnaPersona {
	desocupado :: Bool,
    actividades :: [Actividad],
    ingreso:: Number
    } 
--Quienes cumplen beneficio
realiza :: Actividad -> Persona -> Bool
realiza actividad persona = elem actividad (actividades persona)

ingresoEntre:: Number -> Number -> Persona -> Bool
ingresoEntre minimo maximo persona =  ingreso persona > minimo && ingreso persona < maximo

informaticoMillonario :: Persona -> Bool
informaticoMillonario persona = informatico persona && ingresoEntre 1000000 5000000 persona

informatico :: Persona -> Bool
informatico = realiza "informatico"

promesa1,promesa2,promesa3,promesa4,promesa5,promesa6 :: Promesa
promesa1 = UnaPromesa "nn1" informaticoMillonario 2000
promesa2 = UnaPromesa "nn2" informatico 3000
promesa3 = UnaPromesa "nn3" (\ _ -> True) 9999
promesa4 = UnaPromesa "nn4" (realiza "docente") 1000
promesa5 = UnaPromesa "nn5" (ingresoEntre 0 10000) 1000
promesa6 = UnaPromesa "nn6" (not.realiza "docente") 1000

posibleVoto :: Persona -> [Candidato] -> String
posibleVoto persona candidatos = nombre (mejorCandidato persona candidatos)

--Con recursividad
mejorCandidato :: Persona -> [Candidato] -> Candidato
mejorCandidato _ [candidato] = candidato
mejorCandidato persona (candidato1:candidato2:candidatos)
    | cantidadPromesasRealizables persona candidato1 >= cantidadPromesasRealizables persona candidato2 = mejorCandidato persona (candidato1:candidatos)
    | otherwise = mejorCandidato persona (candidato2:candidatos)
--

--Con fold
mejorCandidato' :: Persona -> [Candidato] -> Candidato
mejorCandidato' persona  = foldl1 (mejorPara persona) 

mejorPara :: Persona -> Candidato -> Candidato -> Candidato
mejorPara persona candidato1 candidato2 
    | cantidadPromesasRealizables persona candidato1 >= cantidadPromesasRealizables persona candidato2 = candidato1
    | otherwise = candidato2
--

cantidadPromesasRealizables :: Persona -> Candidato -> Number
cantidadPromesasRealizables persona candidato = length (promesasRealizables persona candidato)

promesasRealizables :: Persona -> Candidato -> [Promesa]
promesasRealizables persona candidato = filter (esRealizablePara persona candidato) (promesas candidato)

esRealizablePara :: Persona -> Candidato -> Promesa -> Bool
esRealizablePara persona candidato promesa = 
    esRealizable candidato promesa && esBeneficiario promesa persona

esRealizable:: Candidato -> Promesa -> Bool
esRealizable candidato promesa = fmi (partido candidato) < costo promesa

fmi:: String -> Number 
fmi "Jovenes por el Clima" = 1000 
fmi "Urgencia por la Pelota"  = 5000
fmi _ = 0
