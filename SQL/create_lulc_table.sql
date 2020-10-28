DROP 	TABLE IF EXISTS yearly_lulc CASCADE;
--
CREATE	TABLE yearly_lulc(
	cell_id serial NOT NULL,
	lulc smallint,
    burn smallint,
    year smallint
	);
--
COPY 	yearly_lulc
	FROM '{path}'
	DELIMITERS ','
	CSV HEADER;
--
ALTER	TABLE yearly_lulc
	ADD CONSTRAINT cell_id FOREIGN KEY (cell_id) REFERENCES mask_cells (id);

