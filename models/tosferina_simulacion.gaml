model ejemplo

global {
	file building_shapefile <- file("../includes/buildings_ejemplo.shp");
	file road_shapefile <- file("../includes/roads_ejemplo.shp");
	geometry shape <- envelope(building_shapefile) + envelope(road_shapefile);
	graph road_network;
	map<road, float> road_weights;
	float beta_base <- 0.15; // tasa de contagio base S-I por tick
	int incubation_min <- 5;
	int incubation_max <- 21;
	int infectious_days <- 14;
	map<string, float> age_risk <- ['baby'::2.0, 'child'::1.4, 'teen'::1.0, 'adult'::0.8];
	int tick_counter <- 0;
	list<int> daily_new_cases <- [];

	// NUEVOS PARÁMETROS DE CONTAGIO
	bool local_infection <- true; // Activar contagio local
	float neighbours_radius <- 10.0; // Radio de vecinos cercanos

	// NUEVO: control horario simulado
	int current_hour <- 0;

	init {
		create building from: building_shapefile;
		create road from: road_shapefile;
		create people number: 500 {
			location <- any_location_in(one_of(building));
			state <- flip(0.75) ? "ok" : "notok";
		}

		road_weights <- road as_map (each::each.shape.perimeter);
		road_network <- as_edge_graph(road);
	}

	// Reflex para incrementar la hora cada cierto número de ticks (aquí cada 10 ticks)
	reflex update_time when: (cycle mod 10 = 0) {
	current_hour <- (current_hour + 1) mod 24;
	}

	reflex update_road_speed {
		road_weights <- road as_map (each::each.shape.perimeter / each.speed_coeff);
		road_network <- road_network with_weights road_weights;
	}

	reflex daily_stats {
		int nuevos_casos <- length(people where (each.disease_state = "E" and each.infection_day = cycle));
		daily_new_cases <- daily_new_cases + [nuevos_casos];
	}
}

species people skills: [moving] {
	point target;
	float leaving_proba <- 0.05;
	float speed <- rnd(10) #km / #h + 1;
	string state;
	int age <- rnd(0, 80); // años
	string age_band;

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
		if (flip(0.02)) { // 2% infectados al inicio
			disease_state <- "E";
			incubation_period <- rnd(incubation_min, incubation_max);
			infection_day <- tick_counter;
		} else {
			disease_state <- "S";
		}
	}

	bool vaccinated <- flip(0.9); // cobertura 90 %
	int years_since_booster <- rnd(0, 10); // heterogeneidad
	float vax_protection <- max(0, 1 - years_since_booster * 0.42);
	string disease_state <- "S"; // S,E,I,R
	int infection_day <- -1; // tick en que se infectó
	int incubation_period <- 0;

	// NUEVA LÓGICA DE INFECCIÓN (LOCAL O GLOBAL)
	reflex infection when: (disease_state = "S") {
		list<people> infectious_contacts;

		if (local_infection) {
			infectious_contacts <- people at_distance neighbours_radius where (each.disease_state = "I");
		} else {
			int total_people <- length(people);
			int infected_count <- length(people where (each.disease_state = "I"));
			int estimated_contacts <- int(infected_count / total_people * 10);
			infectious_contacts <- [];
			loop i from: 1 to: estimated_contacts {
				people p <- one_of(people where (each.disease_state = "I"));
				if (p != nil) {
					infectious_contacts <- infectious_contacts + [p];
				}
			}

		}

		if (length(infectious_contacts) > 0) {
			float lambda <- 1 - (1 - beta_base) ^ length(infectious_contacts);
			float prob_age <- age_risk[age_band];
			float p_infect <- lambda * prob_age * (1 - vax_protection);
			if flip(p_infect) {
				disease_state <- "E";
				incubation_period <- rnd(incubation_min, incubation_max);
				infection_day <- tick_counter;
			}
		}
	}

	reflex progress when: (disease_state = "E" or disease_state = "I") {
		int days_since_infection <- tick_counter - infection_day;
		if (disease_state = "E" and days_since_infection >= incubation_period) {
			disease_state <- "I"; // pasa a infeccioso
		} else if (disease_state = "I" and days_since_infection >= incubation_period + infectious_days) {
			disease_state <- "R"; // recuperado
			// Waning: volver S después de 5 años (puedes agregar si deseas)
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
			}
		} else if (disease_state = "E") {
			col <- #orange;
		} else if (disease_state = "I") {
			col <- #red;
		} else {
			col <- #green;
		}
		draw circle(5) color: col depth: 4;
	}

	// Nuevo reflex que asigna destino según hora simulada y edad
	reflex schedule_move {
		if (target = nil) {
			// Acceso a la hora global simulada
			int h <- current_hour;

			if (age_band = "baby" or age_band = "child" or age_band = "teen") {
				if (h >= 8 and h < 13) {
					// Ir a colegio (edificios tipo "EDIFICIO EDUCACIONAL")
					building colegio <- one_of(building where (each["cod_otros"] = "EDIFICIO EDUCACIONAL"));
					if (colegio != nil) {
						target <- any_location_in(colegio);
					}
				} else if (h >= 13 and h < 18) {
					// Ir a casa, parque, iglesia
					building destino <- one_of(building where (
						each["cod_otros"] in [ "PARQUE", "TEMPO RELIGIOSO"]
					));
					if (destino != nil) {
						target <- any_location_in(destino);
					}
				} else if (h >= 18) {
					// Ir a casa (cualquier edificio)
					building casa <- one_of(building);
					if (casa != nil) {
						target <- any_location_in(casa);
					}
				}
			} else {
				// Adultos: pueden tener un comportamiento distinto o ir a casa
				building casa <- one_of(building);
				if (casa != nil) {
					target <- any_location_in(casa);
				}
			}
		}
	}

	reflex leave when: (target = nil) and (flip(leaving_proba)) {
		target <- any_location_in(one_of(building));
	}

	reflex move when: target != nil {
		path path_followed <- goto(target: target, on: road_network, recompute_path: false, return_path: true, move_weights: road_weights);
		if (location = target) {
			target <- nil;
		}
	}

	reflex mostrar_edificio when: (target = nil) {
		building b <- one_of(building);
		if b["cod_otros"] != nil {
			write "Estoy en el edificio llamado: " + b["cod_otros"];
		}
	}
}

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

experiment ejemplo type: gui autorun: true {
	float minimum_cycle_duration <- 0.01;

	// PARÁMETROS VISIBLES EN INTERFAZ
	parameter "¿Infección local?" var: local_infection;
	parameter "Radio de vecinos (m)" var: neighbours_radius;

	output synchronized: false {
		display mapa type: 2d axes: false background: rgb(50, 50, 50) fullscreen: false toolbar: false {
			species road refresh: false;
			species building refresh: false;
			species people;
		}
	}
}


