model Ejemplo2

global {
	file building_shapefile <- file("../includes/buildings_ejemplo.shp");
	file road_shapefile <- file("../includes/roads_ejemplo.shp");
	geometry shape <- envelope(building_shapefile) + envelope(road_shapefile);
	graph road_network;
	map<road, float> road_weights;
	float beta_base <- 0.15;
	int incubation_min <- 5;
	int incubation_max <- 21;
	int infectious_days <- 14;
	map<string, float> age_risk <- ['baby'::2.0, 'child'::1.4, 'teen'::1.0, 'adult'::0.8];
	map<string, float> age_mortality <- ['baby'::0.15, 'child'::0.02, 'teen'::0.01, 'adult'::0.03];
	float tick_counter <- 0.0;
	list<int> daily_new_cases <- [];
	list<int> daily_new_infectious <- [];
	list<int> daily_deaths <- [];
	float radio_contagio <- 5.0;
	list<int> daily_baby_infected <- [];
	list<int> daily_child_infected <- [];
	list<int> daily_teen_infected <- [];
	list<int> daily_adult_infected <- [];

	init {
		create building from: building_shapefile;
		create road from: road_shapefile;
		create people number: 100 {
			location <- any_location_in(one_of(building));
			disease_state <- flip(0.1) ? "I" : "S";
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
  if (cycle mod 10 = 0) {
    tick_counter <- tick_counter + 1.0;
  }
}

	reflex update_road_speed {
		road_weights <- road as_map (each::each.shape.perimeter / each.speed_coeff);
		road_network <- road_network with_weights road_weights;
	}

	reflex daily_stats {
		int nuevos_casos <- length(people where (each.disease_state = "E" and each.infection_day = cycle));
		int nuevos_infecciosos <- length(people where (each.disease_state = "I" and (tick_counter - each.infection_day) = each.incubation_period));
		int muertes_hoy <- length(people where (each.disease_state = "D" and each.became_infectious + infectious_days = tick_counter));
		daily_new_cases <- daily_new_cases + [nuevos_casos];
		daily_new_infectious <- daily_new_infectious + [nuevos_infecciosos];
		daily_deaths <- daily_deaths + [muertes_hoy];
		daily_baby_infected <- daily_baby_infected + [length(people where (each.disease_state = "I" and each.age_band = "baby"))];
		daily_child_infected <- daily_child_infected + [length(people where (each.disease_state = "I" and each.age_band = "child"))];
		daily_teen_infected <- daily_teen_infected + [length(people where (each.disease_state = "I" and each.age_band = "teen"))];
		daily_adult_infected <- daily_adult_infected + [length(people where (each.disease_state = "I" and each.age_band = "adult"))];
	}

	reflex terminar_simulacion {
    if (length(people where (each.disease_state = "E" or each.disease_state = "I")) = 0) {
        write "Simulación terminada: sin casos activos.";
        
        // Resumen final por grupo etario
        list<string> grupos <- ["baby", "child", "teen", "adult"];
        loop g over: grupos {
            int fallecidos <- length(people where (each.age_band = g and each.disease_state = "D"));
            int recuperados <- length(people where (each.age_band = g and each.disease_state = "R"));
            write "Grupo: " + g + " → Fallecidos: " + fallecidos + " | Recuperados: " + recuperados;
        }

        do halt;  // Detiene la simulación
    }
}
	

}

species people skills: [moving] {
	point target;
	float leaving_proba <- 0.05;
	float speed <- (rnd(10) + 1) * 10 #km / #h;
	int age <- rnd(0, 80);
	string age_band;
	building edificio_actual <- nil;
	bool vaccinated <- flip(0.9);
	int years_since_booster <- rnd(0, 10);
	float vax_protection <- max(0, 1 - years_since_booster * 0.42);
	string disease_state <- "S";
	int infection_day <- -1;
	int incubation_period <- 0;
	int infectious_days <- 14;
	int became_infectious <- -1;

	init {
		if (age < 5) {
		age_band <- "baby";
		disease_state <- flip(0.15) ? "I" : "S";
	} else if (age < 10) {
		age_band <- "child";
		disease_state <- flip(0.12) ? "I" : "S";
	} else if (age < 20) {
		age_band <- "teen";
		disease_state <- flip(0.08) ? "I" : "S";
	} else {
		age_band <- "adult";
		disease_state <- flip(0.05) ? "I" : "S";
	}

		if (disease_state = "I") {
			infection_day <- 0;
			became_infectious <- 0;
			incubation_period <- 0;
		} }

	reflex validate when: (disease_state = "E") {
		if (incubation_period <= 0) {
		//write "¡Error! Período de incubación no válido: " + incubation_period;
			incubation_period <- incubation_min;
		}

	}

	reflex actualizar_edificio {
		edificio_actual <- first(building overlapping location);
	}

	reflex infection when: (disease_state = "S") {
		list<people> close_contacts <- people at_distance radio_contagio where (each.disease_state = "I");
		list<people> building_contacts <- [];
		if (edificio_actual != nil) {
			building_contacts <- people inside edificio_actual where (each.disease_state = "I" and each != self);
		}

		if (!empty(close_contacts) or !empty(building_contacts)) {
			float p_infect <- 1 - (1 - beta_base) ^ (length(close_contacts) + length(building_contacts)) * age_risk[age_band] * (1 - vax_protection);
			if (flip(p_infect)) {
				disease_state <- "E";
				infection_day <- tick_counter;
				incubation_period <- rnd(incubation_min, incubation_max);
				//write " " + self + " S→E (ciclo " + tick_counter + ")";
			}

		}

	}

	reflex progress when: (disease_state = "E" or disease_state = "I") {
		if (disease_state = "E" and (tick_counter - infection_day) >= incubation_period) {
			disease_state <- "I";
			became_infectious <- tick_counter;
			//write " " + self + " E→I (ciclo " + tick_counter + ")";
		} else if (disease_state = "I" and (tick_counter - became_infectious) >= infectious_days) {
			if (flip(age_mortality[age_band])) {
				disease_state <- "D";
				//write " " + self + " I→D (muere) en ciclo " + tick_counter;
			} else {
				disease_state <- "R";
				//write " " + self + " I→R en ciclo " + tick_counter;
			}

		}

	}

	aspect default {
		rgb col;
		if (disease_state = "S") {
			if (age_band = "baby") {
				col <- #pink;
			} else if (age_band = "child") {
				col <- #yellow;
			} else if (age_band = "teen") {
				col <- #cyan;
			} else {
				col <- #green;
			} } else if (disease_state = "E") {
			col <- #orange;
		} else if (disease_state = "I") {
			col <- #red;
		} else if (disease_state = "R") {
			col <- #gray;
		} else {
			col <- #black;
		}

		draw circle(5) color: col border: #black depth: 4;
		if (disease_state = "E") {
			int restante <- incubation_period - (tick_counter - infection_day);
			//draw string("I en " + max(0, restante)) at: {location.x, location.y + 15} color: #black size: 7;
		} else if (disease_state = "I") {
			int restante <- infectious_days - (tick_counter - became_infectious);
			//draw string("R en " + max(0, restante)) at: {location.x, location.y + 15} color: #black size: 7;
			//draw circle(10) color: rgb(255,0,0,50) border: #red depth: 3;
		} }

	reflex leave when: (target = nil) and (flip(leaving_proba)) {
		target <- any_location_in(one_of(building));
	}

	reflex move when: target != nil {
		path path_followed <- goto(target: target, on: road_network, recompute_path: false, return_path: true, move_weights: road_weights);
		if (location = target) {
			target <- nil;
		} } }

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
		display mapa type: 2d axes: false background: rgb(50, 50, 50) fullscreen: false toolbar: false {
			light #ambient intensity: 128;
			camera 'default' location: {1254.041, 2938.6921, 1792.4286} target: {1258.8966, 1547.6862, 0.0};
			species road refresh: false;
			species building refresh: false;
			species people;
			
		}

		display infectados_edad_serie {
			chart "Evolución por grupo etario" type: series {
				data "Bebés" value: daily_baby_infected color: #pink;
				data "Niños" value: daily_child_infected color: #yellow;
				data "Adolescentes" value: daily_teen_infected color: #cyan;
				data "Adultos" value: daily_adult_infected color: #green;
			}

		}
	
		

		display panel_graficos_baby {
			chart "Evolución Bebés" type: pie {
				data "Susceptibles" value: length(people where (each.disease_state = "S" and each.age_band = "baby")) color: #green;
				data "Expuestos" value: length(people where (each.disease_state = "E" and each.age_band = "baby")) color: #orange;
				data "Infecciosos" value: length(people where (each.disease_state = "I" and each.age_band = "baby")) color: #red;
				data "Recuperados" value: length(people where (each.disease_state = "R" and each.age_band = "baby")) color: #gray;
				data "Muertos" value: length(people where (each.disease_state = "D" and each.age_band = "baby")) color: #black;
			}

		}

		display panel_graficos_child {
			chart "Evolución Niños" type: pie {
				data "Susceptibles" value: length(people where (each.disease_state = "S" and each.age_band = "child")) color: #green;
				data "Expuestos" value: length(people where (each.disease_state = "E" and each.age_band = "child")) color: #orange;
				data "Infecciosos" value: length(people where (each.disease_state = "I" and each.age_band = "child")) color: #red;
				data "Recuperados" value: length(people where (each.disease_state = "R" and each.age_band = "child")) color: #gray;
				data "Muertos" value: length(people where (each.disease_state = "D" and each.age_band = "child")) color: #black;
			}

		}

		display panel_graficos_teen {
			chart "Evolución Adolescentes" type: pie {
				data "Susceptibles" value: length(people where (each.disease_state = "S" and each.age_band = "teen")) color: #green;
				data "Expuestos" value: length(people where (each.disease_state = "E" and each.age_band = "teen")) color: #orange;
				data "Infecciosos" value: length(people where (each.disease_state = "I" and each.age_band = "teen")) color: #red;
				data "Recuperados" value: length(people where (each.disease_state = "R" and each.age_band = "teen")) color: #gray;
				data "Muertos" value: length(people where (each.disease_state = "D" and each.age_band = "teen")) color: #black;
			}

		}

		display panel_graficos_adult {
			chart "Evolución Adultos" type: pie {
				data "Susceptibles" value: length(people where (each.disease_state = "S" and each.age_band = "adult")) color: #green;
				data "Expuestos" value: length(people where (each.disease_state = "E" and each.age_band = "adult")) color: #orange;
				data "Infecciosos" value: length(people where (each.disease_state = "I" and each.age_band = "adult")) color: #red;
				data "Recuperados" value: length(people where (each.disease_state = "R" and each.age_band = "adult")) color: #gray;
				data "Muertos" value: length(people where (each.disease_state = "D" and each.age_band = "adult")) color: #black;
			}

		}

	}

}

