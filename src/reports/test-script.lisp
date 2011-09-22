(rprint (select "Hardware" ("GPU" "Driver" "OpenCL") (path-table)))
(rprint (select "Parameters" ("width" "height") (path-table)))

(with-path "models"
  (rprint (select "models" ("Name" (image "Path") "vertices" "faces" 
				   "BVHBuilding-time (ms)" "TraversingRays-time (ms)" "loading-time (ms)")
		  (path-table))))


