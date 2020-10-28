DROP 	TABLE IF EXISTS mask_cells CASCADE;
--
CREATE	TABLE mask_cells(
	id serial NOT NULL,
	x double precision,
	y double precision,
	PRIMARY KEY (id)
	);
--
COPY 	mask_cells
	FROM '{path}'
	DELIMITERS ','
	CSV HEADER;
--
ALTER TABLE mask_cells ADD COLUMN geom geometry(POINT,4326);
--
UPDATE mask_cells SET geom = ST_SetSRID(ST_MakePoint(x,y),4326);
--
CREATE	INDEX geo_index
	ON mask_cells
	USING GIST (geom);

