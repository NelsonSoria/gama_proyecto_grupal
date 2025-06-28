model Ejemplo2

global {
	file building_shapefile <- file("../includes/buildings_ejemplo.shp");
	file road_shapefile <- file("../includes/roads_ejemplo.shp");
	geometry shape <- envelope(building_shapefile) + envelope(road_shapefile);
	graph road_network;
	map<road, float> road_weights;
	float beta_base <- 0.15; //0.15// tasa de contagio base S-I por tick
	int incubation_min <- 5; //5
	int incubation_max <- 21; //21
	int infectious_days <- 14; //14
	map<string, float> age_risk <- ['baby'::2.0, 'child'::1.4, 'teen'::1.0, 'adult'::0.8];
	float tick_counter <- 0.0;
	list<int> daily_new_cases <- [];
    float radio_contagio <- 5.0; //5.0// Valor inicial (en metros)

	init {
		create building from: building_shapefile;
		create road from: road_shapefile;
		create people number: 100 {
			location <- any_location_in(one_of(building));
			//state <- flip(0.75) ? "ok" : "notok";
			disease_state <- flip(0.1) ? "I" : "S"; // 10% infectados iniciales
	        // SOLO para infectados iniciales
            if (disease_state = "I") {
                infection_day <- 0;
                became_infectious <- 0;
                incubation_period <- 0;
            }
		}

		road_weights <- road as_map (each::each.shape.perimeter);
		road_network <- as_edge_graph(road);
	}
	reflex incrementar_tiempo {
        tick_counter <- tick_counter + 0.01;
         write "--- Ciclo " + tick_counter + " ---";
    }

	reflex update_road_speed {
		road_weights <- road as_map (each::each.shape.perimeter / each.speed_coeff);
		road_network <- road_network with_weights road_weights;
	}

	reflex daily_stats {
		int nuevos_casos <- length(people where (each.disease_state = "E" and each.infection_day = cycle));
		daily_new_cases <- daily_new_cases + [nuevos_casos];
	}
	 list<int> daily_new_infectious <- [];
    
    reflex daily_stats {
        int nuevos_casos <- length(people where (each.disease_state = "E" and each.infection_day = cycle));
        int nuevos_infecciosos <- length(people where (each.disease_state = "I" and (tick_counter - each.infection_day) = each.incubation_period));
        
        daily_new_cases <- daily_new_cases + [nuevos_casos];
        daily_new_infectious <- daily_new_infectious + [nuevos_infecciosos];
    }

}

species people skills: [moving] {
	point target;
	float leaving_proba <- 0.05;
	float speed <- rnd(10) #km / #h + 1;
	string state;
	int age <- rnd(0, 80); // años
	string age_band;
	building edificio_actual <- nil; // Para rastrear el edificio actual
	bool vaccinated <- flip(0.9); // cobertura 90 %
	int years_since_booster <- rnd(0, 10); // heterogeneidad
	float vax_protection <- max(0, 1 - years_since_booster * 0.42);
	string disease_state <- "S"; // S,E,I,R
	int infection_day <- -1; // tick en que se infectó
	int incubation_period <- 0; //rnd(incubation_min, incubation_max);
    int infectious_days <- 14; // Duración fija como infeccioso
	// Atributos clave
    int became_infectious <- -1;  // Nuevo: Ciclo cuando se volvió infeccioso (E→I)
    
    
	init {
		if (age < 5) {
			age_band <- "baby";
		} else if (age < 10) {
			age_band <- "child";
		} else if (age < 20) {
			age_band <- "teen";
		} else {
			age_band <- "adult";
		} 
		// SOLO para infectados iniciales
        if (disease_state = "I") {
            infection_day <- 0;
            became_infectious <- 0; // Infecciosos desde el inicio
            incubation_period <- 0;  // Ya pasaron incubación
        }
		}

	reflex validate when: (disease_state = "E") {
    if (incubation_period <= 0) {
        write "¡Error! Período de incubación no válido: " + incubation_period;
        incubation_period <- incubation_min; // Asignar valor mínimo como fallback
    }
	}
	reflex actualizar_edificio{
        // Actualiza el edificio actual donde se encuentra la persona
        edificio_actual <- first(building overlapping location);
    }
	    // Infección (S → E)
       // Reflex de infección (S → E) - Versión optimizada
    reflex infection when: (disease_state = "S") {
        // Contacto directo
        list<people> close_contacts <- people at_distance radio_contagio where (each.disease_state = "I");
        
        // Contacto en edificio
        list<people> building_contacts <- [];
        if (edificio_actual != nil) {
            building_contacts <- people inside edificio_actual where (each.disease_state = "I" and each != self);
        }
        
        // Calcular probabilidad combinada
        if (!empty(close_contacts) or !empty(building_contacts)) {
            float p_infect <- 1 - (1 - beta_base) ^ (length(close_contacts) + length(building_contacts)) 
                           * age_risk[age_band] * (1 - vax_protection);
            
            if (flip(p_infect)) {
                disease_state <- "E";
                infection_day <- tick_counter;
                incubation_period <- rnd(incubation_min, incubation_max);
                write " "+ self + " S→E (ciclo " + tick_counter + ")";
            }
        }
    }
    
	// UNICO reflex progress (elimina el otro que está duplicado)
    reflex progress when: (disease_state = "E" or disease_state = "I") {
        // E → I
        if (disease_state = "E" and (tick_counter - infection_day) >= incubation_period) {
            disease_state <- "I";
            became_infectious <- tick_counter; // Momento exacto de transición
            write " "+self + " E→I (ciclo " + tick_counter + ") después de " + incubation_period + " ciclos";
        }
        // I → R
        else if (disease_state = "I" and (tick_counter - became_infectious) >= infectious_days) {
            disease_state <- "R";
            write " "+self + " I→R (ciclo " + tick_counter + ") después de " + infectious_days + " ciclos infecciosos";
        }
    }

	aspect default {
    rgb col;
    if (disease_state = "S") {
        if (age_band = "baby") {col <- #pink;}
        else if (age_band = "child") {col <- #yellow;}
        else if (age_band = "teen") {col <- #cyan;}
        else {col <- #green;}
    }
    else if (disease_state = "E") {col <- #orange;}
    else if (disease_state = "I") {col <- #red;}
    else {col <- #gray;}
    
    draw circle(5) color: col border: #black depth: 4;
    
    // Etiquetas mejoradas
    draw string(disease_state) at: {location.x, location.y + 7} color: #black size: 8;
    
    if (disease_state = "E") {
        int restante <- incubation_period - (tick_counter - infection_day);
        draw string("I en " + max(0, restante)) at: {location.x, location.y + 15} color: #black size: 7;
    }
    else if (disease_state = "I") {
        int restante <- infectious_days - (tick_counter - became_infectious);
        draw string("R en " + max(0, restante)) at: {location.x, location.y + 15} color: #black size: 7;
    }
    
    // Destacar infectados
    if (disease_state = "I") {
        draw circle(10) color: rgb(255,0,0,50) border: #red depth: 3;
    }
}

	reflex leave when: (target = nil) and (flip(leaving_proba)) {
		target <- any_location_in(one_of(building));
	}

	reflex move when: target != nil {
		path path_followed <- goto(target: target, on: road_network, recompute_path: false, return_path: true, move_weights: road_weights);
		if (location = target) {
			target <- nil;
		} }

	reflex mostrar_edificio when: (target = nil) {
		building b <- one_of(building);
		if b["cod_otros"] != nil {
			write "Estoy en el edificio llamado: " + b["cod_otros"];
		}

	} }

species building {

	aspect default {
		if (self["cod_otros"] = "EDIFICIO EDUCACIONAL") {
			draw shape color: #red depth: 5;
		} else if (self["cod_otros"] = "PARQUE") {
			draw shape color: #green depth: 5;
		} else {
			draw shape color: darker(#darkgray).darker depth: 2;
		}

	}

}

species road {
	float capacity <- 1 + shape.perimeter / 30;
	int nb_people <- 0 update: length(people at_distance 1);
	float speed_coeff <- 1.0 update: exp(-nb_people / capacity) min: 0.1;
	int buffer <- 10;

	aspect default {
		draw (shape + 5) color: #white;
	}

}

experiment ejemplo type: gui autorun: false {
	float minimum_cycle_duration <- 0.01;
	parameter "Tasa de contagio (β)" var: beta_base min: 0.01 max: 50;
    parameter "Radio de contagio (m)" var: radio_contagio min: 0.1 max: 50.0;
    
	output synchronized: true {
		display mapa type: 3d axes: false background: rgb(50, 50, 50) fullscreen: false toolbar: false {
			light #ambient intensity: 128;
			camera 'default' location: {1254.041, 2938.6921, 1792.4286} target: {1258.8966, 1547.6862, 0.0};
			species road refresh: false;
			species building refresh: false;
			species people;	
		}
		display panel_graficos{
			chart "Evolución de Epidemia" type:pie {
	            data "Susceptibles" value: length(people where (each.disease_state = "S")) color: #green;
			    data "Expuestos" value: length(people where (each.disease_state = "E")) color: #orange;
			    data "Infecciosos" value: length(people where (each.disease_state = "I")) color: #red;
			    data "Recuperados" value: length(people where (each.disease_state = "R")) color: #gray;
    
	        }
	        /*
	        chart "Distribución por Edad" type: histogram position: {500, 750} size: {600, 400} {
		    data "Susceptibles" value: [
		        length(people where (each.disease_state = "S" and each.age_band = "baby")),
		        length(people where (each.disease_state = "S" and each.age_band = "child")),
		        length(people where (each.disease_state = "S" and each.age_band = "teen")),
		        length(people where (each.disease_state = "S" and each.age_band = "adult"))
		    ] color: #green;
    
		    data "Infectados" value: [
		        length(people where (each.disease_state = "I" and each.age_band = "baby")),
		        length(people where (each.disease_state = "I" and each.age_band = "child")),
		        length(people where (each.disease_state = "I" and each.age_band = "teen")),
		        length(people where (each.disease_state = "I" and each.age_band = "adult"))
		    ] color: #red;
			}
			chart "Nuevos Casos" type: histogram position: {1200, 750} size: {600, 400} {
		    data "Nuevos Expuestos" value: daily_new_cases color: #orange;
		    data "Nuevos Infecciosos" value: daily_new_infectious color: #red;
			}¨
			*  */
		}
		
	monitor "Susceptibles" value: length(people where (each.disease_state = "S"));
    monitor "Expuesto" value: length(people where (each.disease_state = "E"));
    monitor "Infectados" value: length(people where (each.disease_state = "I"));
    monitor "Recuperados" value: length(people where (each.disease_state = "R"));
    
    monitor "Promedio I→R" value: mean(people where (each.disease_state = "R") collect (each.became_infectious));
	monitor "Duración E" value: mean(people where (each.disease_state = "I") collect (each.incubation_period));

	}
	
	

}