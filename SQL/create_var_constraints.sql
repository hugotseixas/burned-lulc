ALTER	TABLE monthly_var
	ADD CONSTRAINT cell_id FOREIGN KEY (cell_id) REFERENCES mask_cells (id);

