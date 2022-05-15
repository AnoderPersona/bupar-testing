
# load packages
library(bupaR)
library (edeaR)
library(lubridate)

# Go to the folder where you have the datasets
setwd("C:/Users/Juan Pablo Salazar/Dropbox/Doctorado PUC/Minería de Procesos/Ayudantía bupaR 2022")


read.csv2("ReparacionTelefonos.csv") %>%
  mutate(Start.Timestamp = dmy_hm(Inicio),
         Complete.Timestamp = dmy_hm(Fin)) %>%
  activities_to_eventlog("ID.caso", activity_id = "Actividad", 
                         timestamps = c("Start.Timestamp","Complete.Timestamp"), 
                         resource_id = "Ejecutor"
  ) -> ReparacionTelefonos


# ¿Cuántas ejecuciones del proceso (casos) contiene el log de eventos?

# ¿Cuántas variantes tiene?

summary (ReparacionTelefonos)

ReparacionTelefonos %>% resource_labels()



# ¿Cuáles son las actividades que se realizan más a menudo (o menos)?

ReparacionTelefonos %>%   activities

?activities

activities (ReparacionTelefonos)

# ¿Quiénes son los ejecutores que participan realizando más (o menos) actividades?

ReparacionTelefonos %>%   resources

# ¿Con qué actividad(es) comienza /termina el proceso?

ReparacionTelefonos %>%   start_activities("activity")

ReparacionTelefonos %>%   end_activities("activity")

?end_activities

# ¿Quiénes terminan el proceso?
ReparacionTelefonos %>% end_activities(level = "resource")

ReparacionTelefonos %>% end_activities(level = "resource-activity")


ReparacionTelefonos %>% 
  dotted_chart(
    x = "relative", 
    sort= "duration", 
    color = "Actividad", 
    units = "hours")

ReparacionTelefonos %>% 
  dotted_chart(
    x = "relative", 
    sort= "duration", 
    color = "Ejecutor", 
    units = "hours")

ReparacionTelefonos %>% 
  dotted_chart(
    x = "absolute", 
    sort= "start", 
    color = "Actividad", 
    units = "hours")

# Visualización del DFG - Process_map (ejemplo simple)
ReparacionTelefonos %>%
  process_map(  )



ReparacionTelefonos %>%
  resources()


ReparacionTelefonos %>%
  filter_endpoints(
    end_activities = c("Archivar reparacion") # , reverse =TRUE
  ) %>%
#  resources()
process_map()

ReparacionTelefonos %>% summary()

ReparacionTelefonos %>%
  filter_endpoints(    end_activities = c("Archivar reparacion")   ) %>%
  summary()
#

ReparacionTelefonos %>%
  process_map(
    type_nodes = performance (mean, "min")
  )

ReparacionTelefonos %>%
  filter_endpoints(
    end_activities = c("Archivar reparacion") , reverse =TRUE
    ) %>%
  resources
  process_map()

  # Filtramos la actividad de "Informar al cliente", ya que el DFG no refleja apropiadamente el paralelismo
ReparacionTelefonos %>%
  filter_endpoints(end_activities = c("Archivar reparacion")) %>%
  filter_activity("Informar a cliente", reverse= TRUE) %>% 
  process_map()


ReparacionTelefonos %>%
  filter_endpoints(end_activities = c("Archivar reparacion")) %>%
  filter_activity("Informar a cliente", reverse= TRUE) %>% 
  summary 


ReparacionTelefonos %>%
  filter_endpoints(end_activities = c("Archivar reparacion")) %>%
  filter_activity("Informar a cliente", reverse= TRUE) %>% 
  resources 

# Aplico filtros y de ahí en adelante utilizo log filtrado
ReparacionTelefonos %>%
  filter_endpoints(end_activities = c("Archivar reparacion")) %>%
  filter_activity("Informar a cliente", reverse= TRUE) -> Log_Filtrado 
  
Log_Filtrado %>%  process_map()

Log_Filtrado %>%  resources
  
# Ahora analizamos el desempeño temporal del proceso
Log_Filtrado %>% 
  process_map(
    type_nodes = performance(),
    type_edges = performance(),
    sec_nodes = frequency (),
    sec_edges = frequency()
  )

Log_Filtrado %>% 
  process_map(
     type_nodes = performance(max, "mins"),
     type_edges = performance(max, "mins"),
     sec_nodes = performance (min, "mins"),
     sec_edges = performance (min, "mins")
  )

 
#Throughput  time
# Tiempo total menor o igual a 1 hora
Log_Filtrado %>%
  filter_throughput_time(interval = c(NA,1) , units = "hours") %>%
  process_map(
    type_nodes = performance(),
    type_edges = performance(),
    sec_nodes = frequency (),
    sec_edges = frequency()
  )

# tiempo total >= 1 hora

Log_Filtrado %>%
  filter_throughput_time(interval = c(1, NA) , units = "hours", reverse = TRUE) %>%
  #
  process_map(
    type_nodes = performance(),
    type_edges = performance(),
    sec_nodes = frequency (),
    sec_edges = frequency()
  )

# 10 actividades o menos
Log_Filtrado %>%
  filter_trace_length( interval = c(NA,10)) %>%
  #
  process_map(
    type_nodes = performance(),
    type_edges = performance(),
    sec_nodes = frequency (),
    sec_edges = frequency()
  )

# más de 10 actividades
Log_Filtrado %>%
 # filter_trace_length( interval = c(11, NA)) %>%
  filter_trace_length( interval = c(NA,10), reverse = TRUE) %>%
  #
  trace_explorer(coverage=1)


Log_Filtrado %>%
  filter_trace_length( interval = c(NA,10)) %>%
  trace_explorer (coverage = 1)


#Control Flow
# casos que requieren reparación compleja
Log_Filtrado %>%
  filter_activity_presence(
    activities = "Reparacion (compleja)"
    #    activities = c("Reparacion (compleja)", "Reparacion (simple)"), method="all"
    ) %>%
  process_map(  )

order_to_cash %>% process_map()

# casos que van de reparación simple a compleja
# transición no necesariamente es directa
Log_Filtrado %>%
  filter_precedence(
    antecedents = c("Reparacion (simple)"), 
    consequents = c("Reparacion (compleja)"),
    precedence_type = c("eventually_follows")
  ) %>%
  process_map()

# uso de función filter_trim
# trazas que van desde reparación simple a compleja, por dónde pasan?
Log_Filtrado %>%
  filter_trim(
    start_activities = c("Reparacion (simple)"), 
    end_activities = c("Reparacion (compleja)")
  ) %>%
  process_map()

#--------------
order_to_cash %>% process_map()

# Flujo, incluyendo sólo actividades realizadas al menos 1000 veces
Log_Filtrado %>%
  filter_activity_frequency(interval = c(1000, NA)) %>%
  process_map()

# Mostrar sólo aquellas actividades realizadas por Tester1
Log_Filtrado %>%
  filter_resource (resources = c("Tester1")) %>%
  process_map()

Log_Filtrado %>%
  filter_resource (resources = c("Tester1")) %>%
  process_map(
    type_nodes = performance(),
    sec_nodes = frequency(),
    type_edges = performance()
  )

Log_Filtrado %>% 
  filter_resource (resources = c("Tester1")) %>%
  trace_explorer(coverage = 1)
  
Log_Filtrado %>% 
  filter_resource (resources = c("Tester1")) %>%
  dotted_chart()

# Juntar las actividades asociadas a la reparación en una sola
Log_Filtrado%>%
  process_map()

Log_Filtrado %>%
  act_unite ("Reparar" = c(
    "Reparacion (compleja)", "Reparacion (simple)", 
    "Volver a reparar", "Probar reparacion"
    )) %>%
  process_map()
  
  
#Dashboard para analizar especialización de recursos
ReparacionTelefonos %>%
  resource_dashboard()

# Uso de Reglas
library(processcheckR)
ReparacionTelefonos %>%
  check_rule (ends ("Archivar reparacion"), label = "Archived") %>%
  check_rule (and ("Reparacion (simple)", "Reparacion (compleja)"), label = "Ambas_rep")  -> checked_log

checked_log %>% filter ( Archived &  Ambas_rep) %>%
filter_activity("Informar a cliente", reverse= TRUE) %>% 
  process_map()

#otro ejemplo, 
checked_log %>% 
  filter_activity("Informar a cliente", reverse= TRUE) %>% 
  filter ( Archived & !( Ambas_rep)) %>% process_map()


# Social Network Analysis
Log_Filtrado %>% resources

Log_Filtrado %>%
 filter_resource("Sistema", reverse=TRUE ) %>%
 filter_resource( 
  c("ReparadorS1", "ReparadorC1", "ReparadorC2", 
    "ReparadorS2", "ReparadorS3", "ReparadorC3")) %>%
  resource_map()

Log_Filtrado %>%
  filter_resource("Sistema", reverse=TRUE ) %>%
  filter_resource( 
    c("ReparadorS1", "ReparadorC1", "ReparadorC2", 
      "ReparadorS2", "ReparadorS3", "ReparadorC3")) %>%
  resource_matrix() 

