model ejemplo

global {
	file building_shapefile <- file("../includes/buildings_ejemplo.shp");
	file road_shapefile <- file("../includes/roads_ejemplo.shp");
	geometry shape <- envelope(building_shapefile) + envelope(road_shapefile);

	graph road_network;
	map<road, float> road_weights;

	init {
		create building from: building_shapefile;
		create road from: road_shapefile;

		create people number: 1000 {
			location <- any_location_in(one_of(building));
			state <- flip(0.75) ? "ok" : "notok";
		}

		road_weights <- road as_map (each::each.shape.perimeter);
		road_network <- as_edge_graph(road);
	}

	reflex update_road_speed {
		road_weights <- road as_map (each::each.shape.perimeter / each.speed_coeff);
		road_network <- road_network with_weights road_weights;
	}
	
}



species people skills: [moving] {
	point target;
	float leaving_proba <- 0.05;
	float speed <- rnd(10) #km / #h + 1;
	string state;

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

	if b["cod_otros"] != nil  {
		write "Estoy en el edificio llamado: " + b["cod_otros"];
	}

}
	aspect default {
		 draw circle(5) color: #blue depth: 4;     
   	
	}
}

species building {
	aspect default {
		if (self["cod_otros"] = "EDIFICIO EDUCACIONAL") {
			draw shape color: #red depth: 5; 
		}else if (self["cod_otros"] = "PARQUE"){
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

	output synchronized: true {
		display mapa type: 3d axes: false background: rgb(50, 50, 50) fullscreen: true toolbar: false {
			light #ambient intensity: 128;
			camera 'default' location: {1254.041, 2938.6921, 1792.4286} target: {1258.8966, 1547.6862, 0.0};

			species road refresh: false;
			species building refresh: false;
			species people;
		}
	}
}


