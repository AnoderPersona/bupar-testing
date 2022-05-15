

#install devtools
install.packages("devtools")

library(devtools)
# install lubridate 
install.packages("lubridate")

# install latest version bupar
install.packages("bupaR")
install.packages("edeaR")
install.packages("eventdataR")
install.packages("processmapR")
install.packages("processanimateR")
install.packages("processmonitR")
install.packages("petrinetR")
install.packages("xesreadR")

# install helper package for tutorial
install_github("gertjanssenswillen/bupaRtutorial")

# load bupar
library(bupaR)

# load helper package
library(bupaRtutorial)

# load data
load_all_data()


#Empieza trabajo

read.csv2("PermitLog-2022.csv") %>%
    mutate(Start.Timestamp = dmy_hms(Fin),
           Complete.Timestamp = dmy_hms(Fin)) %>%
    activities_to_eventlog("ID.caso", activity_id = "Actividad", 
                           timestamps = c("Start.Timestamp","Complete.Timestamp"), 
                           resource_id = "RolEjecutor"
    ) -> viajes

#Resumen datos
summary(viajes)


#Respuesta1

#n casos
n_cases(viajes)
#n actividades
n_activities(viajes)
#Actividades mas frecuentes
activities(viajes)
#Personas que participan en el proceso
viajes %>% resource_labels()
#Actividades de inicio
viajes %>%   start_activities("activity")
#Actividades finales
viajes %>%   end_activities("activity")
#Tiemposs
viajes %>% 
    process_map(
        type_edges = performance(max, "days"),
        sec_edges = performance (min, "days"),
        rankdir = "TB"
    )
#Ejecutores que se repiten mas en los procesos
viajes %>%   resources

#Respuesta2

#Visualizacion del process map simple de 7 actividades
viajes %>%
    filter_endpoints(
        end_activities = c("Send Reminder") # , reverse =TRUE
    ) %>%
    #  resources()
    process_map(        
        type_edges = performance(max, "hours"),
        sec_edges = performance (min, "hours"),
        rankdir = "TB")



#Podemos ver que entre cada evento, los tiempos cambian bastante, donde se observa un aumento significante al momento de acercarse al viaje en sí
#El mas frecuente de este caso es el 'send reminder', el cual podria ocurrir mas de una vez
#Las actividades mas importantes a nuestro parecer, son el inicio y el termino del viaje, ya que el proceso entero gira entorno a estos dos eventos.
